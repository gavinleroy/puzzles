#include <cmath>
#include <string>
typedef unsigned int uint;
typedef long long ll;
typedef unsigned long long ull;
typedef unsigned long ul;
typedef double lf;
typedef long double llf;
typedef bool bl;
#define I __attribute__((always_inline))inline
template<typename T> I T max(T a,T b){return a>b?a:b;}
template<typename T> I T min(T a,T b){return a<b?a:b;}
template<typename T> I T abs(T a){return a>0?a:-a;}
#define RC char
#define UC unsigned char
#define OP operator
#define RT return *this;
#define TR *this,x;return x;
#define SS std::string
#define ERR !P.E
#ifdef _WIN32
#define GETC getchar()
#define PUTC(x) putchar(x)
#else 
#define GETC getchar_unlocked()
#define PUTC(x) putchar_unlocked(x)
#endif /* _WIN32 */
struct CHARG{bl E=0;I UC OP()(){RC r=GETC;E=(E||r==EOF);return r;}};
struct CHARP{I void OP()(char x){PUTC(x);}};
#define RX x=0; UC t=P(); while((t<'0'||t>'9')&&t!='-'&&ERR)t=P(); bool f=0;\
if(t=='-')t=P(),f=1; x=t-'0';for(t=P();t>='0'&&t<='9'&&ERR; t=P()) x=x*10+t-'0'
#define RL if(t=='.'){lf u=0.1;for(t=P();t>='0'&&t<='9'&&ERR;t=P(),u*=0.1)x+=u*(t-'0');}if(f)x=-x
#define RU x=0; UC t=P(); while((t<'0'||t>'9')&&ERR)t=P();x=t-'0';\
for(t=P();t>='0'&&t<='9'&&ERR;t=P())x=x*10+t-'0'
I bool IS(char x){ return x==10||x==13||x==' '; }
template<typename T> struct FREAD{T P;I OP bl(){return ERR;}I FREAD&OP,(int&x){RX; if(f)x=-x; RT }I OP int(){int x;TR}I FREAD&OP,(ll & x){RX;if(f)x=-x;RT}I OP ll(){ll x;TR}I FREAD&OP,(char & x){for(x=P();IS(x)&&ERR;x=P());RT}I OP char(){char x;TR}I FREAD&OP,(char* x){UC t=P();for(;IS(t)&&ERR;t=P());if(~t){for(;!IS(t)&&~t&&ERR;t=P())*x++=t;}*x++=0;RT}I FREAD&OP,(lf&x){RX;RL;RT}I OP lf(){lf x;TR}I FREAD&OP,(llf&x){RX;RL;RT}I OP llf() {llf x;TR}I FREAD&OP,(uint&x){RU;RT}I OP uint(){uint x;TR}I FREAD&OP,(ull&x){RU;RT}I OP ull(){ull x;TR}I FREAD&OP,(ul&x){RU;RT}I OP ul(){ul x;TR}};FREAD<CHARG>in;
#define WI(S) if(x){if(x<0)P('-'),x=-x;UC s[S],c=0;while(x)s[c++]=x%10+'0',x/=10;while(c--)P(s[c]);}else P('0')
#define WL if(y){lf t=0.5;for(int i=y;i--;)t*=0.1;if(x>=0)x+=t;else x-=t,P('-');*this,(ll)(abs(x));P('.');if(x<0)\
x=-x;while(y--){x*=10;x-=floor(x*0.1)*10;P(((int)x)%10+'0');}}else if(x>=0)*this,(ll)(x+0.5);else *this,(ll)(x-0.5);
#define WU(S) if(x){UC s[S],c=0;while(x)s[c++]=x%10+'0',x/=10;while(c--)P(s[c]);}else P('0')
template<typename T>struct FWRITE{T P;I FWRITE&OP,(int x){WI(10);RT}I FWRITE&OP()(int x){WI(10);RT}I FWRITE&OP,(uint x){WU(10);RT}I FWRITE&OP()(uint x){WU(10);RT}I FWRITE&OP,(ll x){WI(19);RT}I FWRITE&OP()(ll x){WI(19);RT}I FWRITE&OP,(ull x){WU(20);RT}I FWRITE&OP()(ull x){WU(20);RT}I FWRITE&OP,(ul x){WU(20);RT}I FWRITE&OP()(ul x){WU(20);RT}I FWRITE&OP,(char x){P(x);RT}I FWRITE&OP()(char x){P(x);RT}I FWRITE&OP,(const char*x){while(*x)P(*x++);RT}I FWRITE&OP()(const char*x){while(*x)P(*x++);RT}I FWRITE&OP()(const char*x,int n){while(*x&&n--)P(*x++);RT}I FWRITE&OP()(const SS&s,int n){int t=n;while(t--)*this,s[n-t-1];RT}I FWRITE&OP,(const SS&x){*this,x.c_str();RT}I FWRITE&OP()(const SS&x){*this(x.c_str());RT}I FWRITE&OP()(lf x,int y){WL;RT}I FWRITE&OP()(llf x,int y){WL;RT}};FWRITE<CHARP>out;
// DONE I/O ------------------------------------------------------------------------------------------------------>
#include <utility>
#include <algorithm>
#include <vector>
#define NL '\n'
#define PII pair<int,int>
#define MP(x,y) make_pair(x,y)
#define F first
#define S second

using namespace std;

int n,m,goal[20],groups[20],from_to[20][20];
vector<int> adj[20];

void init(){ // Read in data, set up graphs
	in,n,m;
	for(int i=0;i<n;i++){
		in,goal[i];     // Read in goal array
		goal[i]--;
		adj[i].clear(); // Clear the adjacency list for robot swaps
		for(int j=0;j<n;j++) from_to[i][j]=0; // Initialize from_to matrix
	}	
	for(int i=0,x,y;i<m;i++){
		in,x,y; x--,y--;
		adj[x].push_back(y); adj[y].push_back(x);
	}
}

// Recursively assign group by adj list
void dfs_assign(int v, bool visited[], int group){
	groups[v] = group;
	visited[v]=true;
	for(int i=0;i<adj[v].size();i++)
		if(!visited[adj[v][i]])
			dfs_assign(adj[v][i], visited, group);
}

// Group each connected component and give it a number
int find_conn_comp(){
	bool visited[20]; for(int i=0;i<20;i++) visited[i]=0;

	int group=0;
	for(int i=0;i<n;i++){
		if(!visited[i]){
			dfs_assign(i, visited, group);
			group++;
		}
	}
	return group;
}

int fill_from_to(){
	int ret=0;
	for(int i=0;i<n;i++)
		if(groups[i] != groups[goal[i]])
			from_to[groups[i]][groups[goal[i]]]++,ret++;
	return ret;
}

int remove_cycle(int source, int parent[]){
	int f = parent[source], t = source, ret=0;
	while(f >= 0){
		from_to[f][t]--, ret++, parent[t] = -1;
		t = f;
		f = parent[f];
	}
	return ret;
}

int find_cycle(int from, int to, int numg, int parent[]){
	if(parent[to] != -1){
		parent[to] = from;
	       	return remove_cycle(to, parent);
	}
	parent[to] = from;
	int i,ff[20]; for(int i=0;i<numg;i++) ff[i]=i; random_shuffle(ff, ff+numg);
	for(int q=0;q<numg;q++){
		i = ff[q];
		if(from_to[to][i]) return find_cycle(to, i, numg, parent);
	}
	return 0;
}

int find_cycle(int numg){
	int i,j;
	int parent[20]; for(int i=0;i<20;i++) parent[i]=-1;
	int ff[20]; for(int i=0;i<numg;i++) ff[i]=i; random_shuffle(ff, ff+numg);
	int ss[20]; for(int i=0;i<numg;i++) ss[i]=i; random_shuffle(ss, ss+numg);

	for(int q=0;q<numg;q++){
		i=ff[q];	
		for(int p=0;p<numg;p++){
			j=ss[p];
			if(from_to[i][j]) return find_cycle(i, j, numg, parent);
		}
	}
	return 0;
}

int solve(){
	// Find the connected components in robot swapping graph
	int num_groups = find_conn_comp();
	// Count the number of vases that need to go from group i to group j where 0<=i,j<=#groups
	int displaced = fill_from_to();
	int ans=0, csize;
	if(displaced){
		// csize represents the cycle size and it 
		// should be noted that a cycle of size n
		// can be solved in n-1 swaps
		while( (csize=find_cycle(num_groups)) > 0 ) ans += csize-1;
	}
	return ans;
}

int main(){
#ifdef LOCAL
	freopen("input.1", "r", stdin);
#endif
	srand((unsigned)time(0));
	for(int T=in;T--;){
		int ans=~(1<<31);
		init();
		// Play off statistics a little to find right answer
		for(int i=0;i<5;i++) ans = ::min(ans, solve());
		out,ans,NL;
	}
	return 0;
}
