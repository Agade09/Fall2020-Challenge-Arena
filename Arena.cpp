#include <iostream>
#include <iomanip>
#include <vector>
#include <array>
#include <list>
#include <cmath>
#include <fstream>
#include <random>
#include <chrono>
#include <sstream>
#include <unistd.h>
#include <sys/wait.h>
#include <ext/stdio_filebuf.h>
#include <sys/ioctl.h>
#include <poll.h>
#include <omp.h>
#include <algorithm>
using namespace std;
using namespace std::chrono;

constexpr bool tests{true};
constexpr bool Debug_AI{false},Timeout{false};
constexpr double FirstTurnTime{1*(Timeout?1:10)},TimeLimit{0.050*(Timeout?1:10)};
constexpr int PIPE_READ{0},PIPE_WRITE{1};
constexpr int N{2};

constexpr int Max_Ingredients{10};

bool stop{false};//Global flag to stop all arena threads when SIGTERM is received
default_random_engine generator(system_clock::now().time_since_epoch().count());

struct recipe{
    array<int,4> cost;
    int id,payout,bonus;
};

ostream& operator<<(ostream &os,const recipe &r){
    for(const int c:r.cost){
        os << c << " ";
    }
    os << r.id << " " << r.payout << " " << r.bonus;
    return os;
}

istream& operator>>(istream &is,recipe &r){
    for(int &c:r.cost){
        is >> c;
    }
    is >> r.id >> r.payout >> r.bonus;
    return is;
}

struct recipe_concept{
    array<int,4> cost;
    int payout;
};

struct spell{
    array<int,4> cost;
    int id;
    bool castable,repeatable;
};

ostream& operator<<(ostream &os,const spell &s){
    for(const int c:s.cost){
        os << c << " ";
    }
    os << s.id << " " << s.castable << " " << s.repeatable;
    return os;
}

istream& operator>>(istream &is,spell &s){
    for(int &c:s.cost){
        is >> c;
    }
    is >> s.id >> s.castable >> s.repeatable;
    return is;
}

struct tome_spell{
    spell s;
    int id,price,tax;
};

ostream& operator<<(ostream &os,const tome_spell &t){
    os << t.s << " " << t.id << " " << t.price << " " << t.tax;
    return os;
}

istream& operator>>(istream &is,tome_spell &t){
    is >> t.s >> t.id >> t.price >> t.tax;
    return is;
}

enum action_type{
    BREW,CAST,LEARN,REST
};

struct action{
    action_type type;
    int idx,times;
};

struct state{
    int turn{0};
    array<array<int,4>,2> inv;
    array<int,2> score;
    vector<tome_spell> tome;
    array<int,2> Brew_Count{0,0};
    array<int,2> Bonus_Count;
    array<vector<spell>,2> spells;
    vector<recipe> recipes;
    bool game_over()const{
        //return false;
        return any_of(Brew_Count.begin(),Brew_Count.end(),[](const int c){return c>=6;}) || turn>100;
    }
    int winner()const{
        const array<int,2> Final_Scores{final_score(0),final_score(1)};
        return Final_Scores[0]>Final_Scores[1]?1:(Final_Scores[0]<Final_Scores[1]?-1:0);
    }
    int final_score(const int player_id)const{
        return score[player_id]+accumulate(next(inv[player_id].begin(),1),inv[player_id].end(),0);
    }
    void Remove_Urgency_Bonus(){
        for_each(recipes.begin(),recipes.end(),[](recipe &r){
            r.payout-=r.bonus;
            r.bonus=0;
        });
    }
    void Apply_Urgency_Bonus(){
        int head{0};
        if(Bonus_Count[0]>0){
            recipes[head].payout+=3;
            recipes[head].bonus=3;
            ++head;
        }
        if(Bonus_Count[1]>0){
            recipes[head].payout+=1;
            recipes[head].bonus=1;
        }
    }
    void refresh_tome_prices(){
        for(int i=0;i<min(6,static_cast<int>(tome.size()));++i){
            tome[i].price=i;
        }
    }
};

void Apply_Action(state &S,const action &mv,const int player_id){
    if(mv.type==REST){
        for(spell &s:S.spells[player_id]){
            s.castable=true;
        }
    }
    else if(mv.type==BREW){
        const recipe &r{S.recipes[mv.idx]};
        for(int i=0;i<4;++i){
            S.inv[player_id][i]-=r.cost[i];
        }
        S.score[player_id]+=r.payout;
        ++S.Brew_Count[player_id];
        if(r.bonus==3){
            --S.Bonus_Count[0];
        }
        else if(r.bonus==1){
            --S.Bonus_Count[1];
        }
    }
    else if(mv.type==CAST){
        spell &s{S.spells[player_id][mv.idx]};
        for(int i=0;i<4;++i){
            S.inv[player_id][i]-=s.cost[i]*mv.times;
        }
        s.castable=false;
    }
    else{//LEARN
        tome_spell &t{S.tome[mv.idx]};
        S.spells[player_id].push_back(t.s);
        S.inv[player_id][0]-=t.price;
        S.inv[player_id][0]+=min(t.tax,Max_Ingredients-accumulate(S.inv[player_id].begin(),S.inv[player_id].end(),0));
    }
}

void simulate(state &S,const array<action,2> &Actions){
    for(int player_id=0;player_id<2;++player_id){
        Apply_Action(S,Actions[player_id],player_id);
    }
    for(const action &mv:Actions){
        if(mv.type==LEARN){
            S.tome[mv.idx].price=999;
        }
        else if(mv.type==BREW){
            S.recipes[mv.idx].cost[0]=999;
        }
    }
    S.tome.erase(remove_if(S.tome.begin(),S.tome.end(),[](const tome_spell &t){return t.price>=999;}),S.tome.end());
    S.recipes.erase(remove_if(S.recipes.begin(),S.recipes.end(),[](const recipe &r){return r.cost[0]>=999;}),S.recipes.end());
    for(const action &mv:Actions){
        if(mv.type==LEARN){
            for(int i=0;i<min(mv.idx,static_cast<int>(S.tome.size()));++i){
                ++S.tome[i].tax;
            }
        }
    }
    S.refresh_tome_prices();
    S.Remove_Urgency_Bonus();
    S.Apply_Urgency_Bonus();
    ++S.turn;
}

struct AI{
    int id,pid,outPipe,errPipe,inPipe,turnOfDeath;
    string name;
    inline void stop(const int turn=-1){
        if(alive()){
            kill(pid,SIGTERM);
            int status;
            waitpid(pid,&status,0);//It is necessary to read the exit code for the process to stop
            if(!WIFEXITED(status)){//If not exited normally try to "kill -9" the process
                kill(pid,SIGKILL);
            }
            turnOfDeath=turn;
        }
    }
    inline bool alive()const{
        return kill(pid,0)!=-1;//Check if process is still running
    }
    inline void Feed_Inputs(const string &inputs){
        if(write(inPipe,&inputs[0],inputs.size())!=inputs.size()){
            throw(5);
        }
    }
    inline ~AI(){
        close(errPipe);
        close(outPipe);
        close(inPipe);
        stop();
    }
};

void StartProcess(AI &Bot){
    int StdinPipe[2];
    int StdoutPipe[2];
    int StderrPipe[2];
    if(pipe(StdinPipe)<0){
        perror("allocating pipe for child input redirect");
    }
    if(pipe(StdoutPipe)<0){
        close(StdinPipe[PIPE_READ]);
        close(StdinPipe[PIPE_WRITE]);
        perror("allocating pipe for child output redirect");
    }
    if(pipe(StderrPipe)<0){
        close(StderrPipe[PIPE_READ]);
        close(StderrPipe[PIPE_WRITE]);
        perror("allocating pipe for child stderr redirect failed");
    }
    int nchild{fork()};
    if(nchild==0){//Child process
        if(dup2(StdinPipe[PIPE_READ],STDIN_FILENO)==-1){// redirect stdin
            perror("redirecting stdin");
            return;
        }
        if(dup2(StdoutPipe[PIPE_WRITE],STDOUT_FILENO)==-1){// redirect stdout
            perror("redirecting stdout");
            return;
        }
        if(dup2(StderrPipe[PIPE_WRITE],STDERR_FILENO)==-1){// redirect stderr
            perror("redirecting stderr");
            return;
        }
        close(StdinPipe[PIPE_READ]);
        close(StdinPipe[PIPE_WRITE]);
        close(StdoutPipe[PIPE_READ]);
        close(StdoutPipe[PIPE_WRITE]);
        close(StderrPipe[PIPE_READ]);
        close(StderrPipe[PIPE_WRITE]);
        execl(Bot.name.c_str(),Bot.name.c_str(),(char*)NULL);//(char*)Null is really important
        //If you get past the previous line its an error
        perror("exec of the child process");
    }
    else if(nchild>0){//Parent process
        close(StdinPipe[PIPE_READ]);//Parent does not read from stdin of child
        close(StdoutPipe[PIPE_WRITE]);//Parent does not write to stdout of child
        close(StderrPipe[PIPE_WRITE]);//Parent does not write to stderr of child
        Bot.inPipe=StdinPipe[PIPE_WRITE];
        Bot.outPipe=StdoutPipe[PIPE_READ];
        Bot.errPipe=StderrPipe[PIPE_READ];
        Bot.pid=nchild;
    }
    else{//failed to create child
        close(StdinPipe[PIPE_READ]);
        close(StdinPipe[PIPE_WRITE]);
        close(StdoutPipe[PIPE_READ]);
        close(StdoutPipe[PIPE_WRITE]);
        perror("Failed to create child process");
    }
}

inline string EmptyPipe(const int fd){
    int nbytes;
    if(ioctl(fd,FIONREAD,&nbytes)<0){
        throw(4);
    }
    string out;
    out.resize(nbytes);
    if(read(fd,&out[0],nbytes)<0){
        throw(4);
    }
    return out;
}

bool IsValidMove(const state &S,const AI &Bot,const string &Move){
	stringstream ss(Move);
	string action_type;
	if(!(ss >> action_type)){
		return false;
	}
    if(action_type!="REST" && action_type!="WAIT"){
        int id;
        if(!(ss >> id)){
            return false;
        }
    }
	return true;
}

string GetMove(const state &S,AI &Bot,const int turn){
    pollfd outpoll{Bot.outPipe,POLLIN};
    time_point<system_clock> Start_Time{system_clock::now()};
    string out;
    while(static_cast<duration<double>>(system_clock::now()-Start_Time).count()<(turn==1?FirstTurnTime:TimeLimit) && !IsValidMove(S,Bot,out)){
        double TimeLeft{(turn==1?FirstTurnTime:TimeLimit)-static_cast<duration<double>>(system_clock::now()-Start_Time).count()};
        if(poll(&outpoll,1,TimeLeft)){
            out+=EmptyPipe(Bot.outPipe);
        }
    }
    return out;
}

action StringToAction(const string &mv_str,const state &S,const int player_id){
	stringstream ss(mv_str);
	action mv;
    string action_type;
	ss >> action_type;
    if(action_type=="REST" || action_type=="WAIT"){
        mv.type=REST;
    }
    else if(action_type=="BREW"){
        mv.type=BREW;
    }
    else if(action_type=="CAST"){
        mv.type=CAST;
    }
    else if(action_type=="LEARN"){
        mv.type=LEARN;
    }
    else{
        throw(3);
    }
    int id;
    if(ss && mv.type!=REST){
        ss >> id;
        if(action_type=="BREW"){
            mv.idx=distance(S.recipes.begin(),find_if(S.recipes.begin(),S.recipes.end(),[&](const recipe &r){return r.id==id;}));
            if(mv.idx>=S.recipes.size()){
                throw(3);
            }
        }
        else if(action_type=="CAST"){
            mv.idx=distance(S.spells[player_id].begin(),find_if(S.spells[player_id].begin(),S.spells[player_id].end(),[&](const spell &s){return s.id==id;}));
            if(mv.idx>=S.spells[player_id].size()){
                throw(3);
            }
        }
        else if(action_type=="LEARN"){
            mv.idx=distance(S.tome.begin(),find_if(S.tome.begin(),S.tome.end(),[&](const tome_spell &t){return t.id==id;}));
            if(mv.idx>=S.tome.size()){
                throw(3);
            }
        }
    }
    if(ss && mv.type==CAST){
        ss >> mv.times;
    }
	return mv;
}

inline bool Has_Won(const array<AI,N> &Bot,const int idx)noexcept{
    if(!Bot[idx].alive()){
        return false;
    }
    for(int i=0;i<N;++i){
        if(i!=idx && Bot[i].alive()){
            return false;
        }
    }
    return true;
}

inline bool All_Dead(const array<AI,N> &Bot)noexcept{
    for(const AI &b:Bot){
        if(b.alive()){
            return false;
        }
    }
    return true;
}

int Play_Game(const array<string,N> &Bot_Names,state &S){
	array<AI,N> Bot;
	int turn{0};
	for(int i=0;i<N;++i){
		Bot[i].id=i;
		Bot[i].name=Bot_Names[i];
		StartProcess(Bot[i]);
	}
	while(++turn>0){
        //cerr << "Turn " << turn << endl;
        array<action,2> Actions;
        for(int id=0;id<2;++id){
            if(Bot[id].alive()){
                //Feed turn inputs
                string bot_out;
                stringstream ss;
                const int N_Actions{5+S.spells[0].size()+S.spells[1].size()+min(6,static_cast<int>(S.tome.size()))};
                ss << N_Actions << endl;
                for(int recipe_idx=0;recipe_idx<5;++recipe_idx){
                    const recipe &r{S.recipes[recipe_idx]};
                    ss << r.id << " " << "BREW" << " ";
                    for(int i=0;i<4;++i){
                        ss << -r.cost[i] << " ";
                    }
                    ss << r.payout << " " << r.bonus << " " << (r.bonus==3?S.Bonus_Count[0]:(r.bonus==1?S.Bonus_Count[1]:0)) << " " << 0 << " " << 0 << endl;
                }
                for(int id2=0;id2<2;++id2){
                    for(const spell &s:S.spells[id2]){
                        ss << s.id << " " << (id2==id?"CAST":"OPPONENT_CAST") << " ";
                        for(int i=0;i<4;++i){
                            ss << -s.cost[i] << " ";
                        }
                        ss << 0 << " " << -1 << " " << 0 << " " << s.castable << " " << s.repeatable << endl;
                    }
                }
                for(int tome_idx=0;tome_idx<min(6,static_cast<int>(S.tome.size()));++tome_idx){
                    const tome_spell &t{S.tome[tome_idx]};
                    ss << t.id << " " << "LEARN" << " ";
                    for(int i=0;i<4;++i){
                        ss << -t.s.cost[i] << " ";
                    }
                    ss << 0 << " " << t.price << " " << t.tax << " " << false << " " << t.s.repeatable << endl;
                }
                for(int id_delta=0;id_delta<2;++id_delta){
                    const int id2{(id+id_delta)%2};
                    for(int i=0;i<4;++i){
                        ss << S.inv[id2][i] << " ";
                    }
                    ss << S.score[id2] << endl;
                }
                try{
                    //cerr << ss.str();
                    Bot[id].Feed_Inputs(ss.str().c_str());
                    bot_out=GetMove(S,Bot[id],turn);
                    Actions[id]=StringToAction(bot_out,S,id);
                    //cerr << "Type " << Actions[id].type << " idx " << Actions[id].idx << " times " << Actions[id].times << endl;
                    string err_str{EmptyPipe(Bot[id].errPipe)};
                    if(Debug_AI){
                        ofstream err_out("log.txt",ios::app);
                        err_out << err_str << endl;
                    }
                }
                catch(int ex){
                    if(ex==1){//Timeout
                        cerr << "Loss by Timeout of AI " << Bot[id].id << " name: " << Bot[id].name << endl;
                    }
                    else if(ex==3){
                        cerr << "Invalid move from AI " << Bot[id].id << " name: " << Bot[id].name << " bot_out " << bot_out << endl;
                    }
                    else if(ex==4){
                        cerr << "Error emptying pipe of AI " << Bot[id].name << endl;
                    }
                    else if(ex==5){
                        cerr << "AI " << Bot[id].name << " died before being able to give it inputs" << endl;
                    }
                    Bot[id].stop(turn);
                }
            }
        }
		if(All_Dead(Bot)){
            return -1;//Draw
        }
        else{
        	for(int i=0;i<N;++i){
        		if(Has_Won(Bot,i)){
        			return i;
        		}
        	}
        }
		simulate(S,Actions);
        if(S.game_over()){
            //cerr << S.score[0] << " " << S.score[1] << endl;
        	return S.winner()==1?0:(S.winner()==-1?1:-1);
        }
	}
	throw(0);
}

state Random_Initial_State(){//https://github.com/CodinGame/FallChallenge2020/blob/main/src/main/java/com/codingame/game/Deck.java
	state S;
    int unique_id{0};
    constexpr array<recipe_concept,36> All_Recipes{recipe_concept{{2, 2, 0, 0},6},recipe_concept{{3, 2, 0, 0},7},recipe_concept{{0, 4, 0, 0},8},recipe_concept{{2, 0, 2, 0},8},recipe_concept{{2, 3, 0, 0},8},recipe_concept{{3, 0, 2, 0},9},recipe_concept{{0, 2, 2, 0},10},recipe_concept{{0, 5, 0, 0},10},recipe_concept{{2, 0, 0, 2},10},recipe_concept{{2, 0, 3, 0},11},recipe_concept{{3, 0, 0, 2},11},recipe_concept{{0, 0, 4, 0},12},recipe_concept{{0, 2, 0, 2},12},recipe_concept{{0, 3, 2, 0},12},recipe_concept{{0, 2, 3, 0},13},recipe_concept{{0, 0, 2, 2},14},recipe_concept{{0, 3, 0, 2},14},recipe_concept{{2, 0, 0, 3},14},recipe_concept{{0, 0, 5, 0},15},recipe_concept{{0, 0, 0, 4},16},recipe_concept{{0, 2, 0, 3},16},recipe_concept{{0, 0, 3, 2},17},recipe_concept{{0, 0, 2, 3},18},recipe_concept{{0, 0, 0, 5},20},recipe_concept{{2, 1, 0, 1},9},recipe_concept{{0, 2, 1, 1},12},recipe_concept{{1, 0, 2, 1},12},recipe_concept{{2, 2, 2, 0},13},recipe_concept{{2, 2, 0, 2},15},recipe_concept{{2, 0, 2, 2},17},recipe_concept{{0, 2, 2, 2},19},recipe_concept{{1, 1, 1, 1},12},recipe_concept{{3, 1, 1, 1},14},recipe_concept{{1, 3, 1, 1},16},recipe_concept{{1, 1, 3, 1},18},recipe_concept{{1, 1, 1, 3},20}};
	for(const recipe_concept &r:All_Recipes){
        S.recipes.push_back({r.cost,unique_id++,r.payout,0});
    }
    shuffle(S.recipes.begin(),S.recipes.end(),generator);
    constexpr array<array<int,4>,42> All_Spells{array<int,4>{-3, 0, 0, 1},array<int,4>{3, -1, 0, 0},array<int,4>{1, 1, 0, 0},array<int,4>{0, 0, 1, 0},array<int,4>{3, 0, 0, 0},array<int,4>{2, 3, -2, 0},array<int,4>{2, 1, -2, 1},array<int,4>{3, 0, 1, -1},array<int,4>{3, -2, 1, 0},array<int,4>{2, -3, 2, 0},array<int,4>{2, 2, 0, -1},array<int,4>{-4, 0, 2, 0},array<int,4>{2, 1, 0, 0},array<int,4>{4, 0, 0, 0},array<int,4>{0, 0, 0, 1},array<int,4>{0, 2, 0, 0},array<int,4>{1, 0, 1, 0},array<int,4>{-2, 0, 1, 0},array<int,4>{-1, 0, -1, 1},array<int,4>{0, 2, -1, 0},array<int,4>{2, -2, 0, 1},array<int,4>{-3, 1, 1, 0},array<int,4>{0, 2, -2, 1},array<int,4>{1, -3, 1, 1},array<int,4>{0, 3, 0, -1},array<int,4>{0, -3, 0, 2},array<int,4>{1, 1, 1, -1},array<int,4>{1, 2, -1, 0},array<int,4>{4, 1, -1, 0},array<int,4>{-5, 0, 0, 2},array<int,4>{-4, 0, 1, 1},array<int,4>{0, 3, 2, -2},array<int,4>{1, 1, 3, -2},array<int,4>{-5, 0, 3, 0},array<int,4>{-2, 0, -1, 2},array<int,4>{0, 0, -3, 3},array<int,4>{0, -3, 3, 0},array<int,4>{-3, 3, 0, 0},array<int,4>{-2, 2, 0, 0},array<int,4>{0, 0, -2, 2},array<int,4>{0, -2, 2, 0},array<int,4>{0, 0, 2, -1}};
    for(const array<int,4> &delta:All_Spells){
        S.tome.push_back({spell{{-delta[0],-delta[1],-delta[2],-delta[3]},unique_id++,true,any_of(delta.begin(),delta.end(),[](const int c){return c<0;})},unique_id++,0,0});
    }
    shuffle(S.tome.begin(),S.tome.end(),generator);
    S.refresh_tome_prices();
    const vector<array<int,4>> Beginning_Spells{{2,0,0,0},{-1,1,0,0},{0,-1,1,0},{0,0,-1,1}};
    for(int player_id=0;player_id<2;++player_id){
        for(const array<int,4> &delta:Beginning_Spells){
            S.spells[player_id].push_back(spell{{-delta[0],-delta[1],-delta[2],-delta[3]},unique_id++,true,false});
        }
        fill(S.inv[player_id].begin(),S.inv[player_id].end(),0);
        S.inv[player_id][0]=3;
    }
    fill(S.score.begin(),S.score.end(),0);
    fill(S.Bonus_Count.begin(),S.Bonus_Count.end(),4);
    S.Apply_Urgency_Bonus();
	return S;
}

array<float,N> Play_Round(const array<string,N> &Bot_Names){
	array<float,N> Points;
	fill(Points.begin(),Points.end(),0);
	for(int i=0;i<N;++i){
		state S{Random_Initial_State()};
		array<string,N> Bot_Names2{Bot_Names};
		rotate(Bot_Names2.begin(),next(Bot_Names2.begin(),i),Bot_Names2.end());
		const int winner{Play_Game(Bot_Names2,S)};
		if(winner==-1){
			for(float &p:Points){
				p+=0.5;
			}
		}
		else{
			Points[(winner+i)%N]+=1;
            //Points[winner]+=1;
		}
	}
	return Points;
}

void StopArena(const int signum){
    stop=true;
}

int main(int argc,char **argv){
    if(argc<3){
        cerr << "Program takes 2 inputs, the names of the AIs fighting each other" << endl;
        return 0;
    }
    int N_Threads{1};
    if(argc>=4){//Optional N_Threads parameter
        N_Threads=min(2*omp_get_num_procs(),max(1,atoi(argv[3])));
        cerr << "Running " << N_Threads << " arena threads" << endl;
    }
    array<string,N> Bot_Names;
    for(int i=0;i<2;++i){
        Bot_Names[i]=argv[i+1];
    }
    cout << "Testing AI " << Bot_Names[0];
    for(int i=1;i<N;++i){
        cerr << " vs " << Bot_Names[i];
    }
    cerr << endl;
    for(int i=0;i<N;++i){//Check that AI binaries are present
        ifstream Test{Bot_Names[i].c_str()};
        if(!Test){
            cerr << Bot_Names[i] << " couldn't be found" << endl;
            return 0;
        }
        Test.close();
    }
    signal(SIGTERM,StopArena);//Register SIGTERM signal handler so the arena can cleanup when you kill it
    signal(SIGPIPE,SIG_IGN);//Ignore SIGPIPE to avoid the arena crashing when an AI crashes
    int games{0},draws{0};
    array<double,2> points{0,0};
    #pragma omp parallel num_threads(N_Threads) shared(games,points,Bot_Names)
    while(!stop){
        array<float,N> round_points{Play_Round(Bot_Names)};
        for(int i=0;i<N;++i){
        	#pragma omp atomic
        	points[i]+=round_points[i];
        }
        #pragma omp atomic
        games+=2;
        double p{static_cast<double>(points[0])/games};
        double sigma{sqrt(p*(1-p)/games)};
        double better{0.5+0.5*erf((p-0.5)/(sqrt(2)*sigma))};
        #pragma omp critical
        cout << "Wins:" << setprecision(4) << 100*p << "+-" << 100*sigma << "% Games:" << games << " Draws:" << draws << " " << better*100 << "% chance that " << Bot_Names[0] << " is better" << endl;
    }
}