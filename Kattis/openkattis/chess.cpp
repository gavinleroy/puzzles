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
template<typename T>struct FWRITE{T P;I FWRITE&OP,(int x){WI(10);RT}I FWRITE&OP()(int x){WI(10);RT}I FWRITE&OP,(uint x){WU(10);RT}I FWRITE&OP()(uint x){WU(10);RT}I FWRITE&OP,(ll x){WI(19);RT}I FWRITE&OP()(ll x){WI(19);RT}I FWRITE&OP,(ull x){WU(20);RT}I FWRITE&OP()(ull x){WU(20);RT}I FWRITE&OP,(ul x){WU(20);RT}I FWRITE&OP()(ul x){WU(20);RT}I FWRITE&OP,(char x){P(x);RT}I FWRITE&OP()(char x){P(x);RT}I FWRITE&OP,(const char*x){while(*x)P(*x++);RT}I FWRITE&OP()(const char*x){while(*x)P(*x++);RT}I FWRITE&OP,(const SS & x){*this,x.c_str();RT}I FWRITE&OP()(const SS & x){*this(x.c_str());RT}I FWRITE&OP()(lf x,int y){WL;RT}I FWRITE&OP()(llf x,int y){WL;RT}};FWRITE<CHARP>out;
// DONE I/O ------------------------------------------------------------------------------------------------------>
#include <utility>
#include <algorithm>
#include <vector>
#include <set>
#include <unordered_set>
#define NL '\n'
#define PII pair<int,int>
#define VPII vector<PII >
#define MP(x,y) make_pair(x,y)
#define F first
#define S second

using namespace std;

void generate(VPII & ss, int c, int r){
	// Generate upper left
	for(int i=c-1,j=r-1;i>=0&&j>0;i--,j--) ss.push_back(MP(i,j));
	// Generate upper right
	for(int i=c+1,j=r-1;i<8&&j>0;i++,j--) ss.push_back(MP(i,j));
	// Generate lower left
	for(int i=c-1,j=r+1;i>=0&&j<=8;i--,j++) ss.push_back(MP(i,j));
	// Generate lower right
	for(int i=c+1,j=r+1;i<8&&j<=8;i++,j++) ss.push_back(MP(i,j));
}

bool findint(VPII & s1, VPII & s2, PII & p){
	for(int i=0;i<s1.size();i++){
		for(int j=0;j<s2.size();j++){
			if(s1[i].F==s2[j].F&&s1[i].S==s2[j].S){ 
				p=s1[i]; 
				return true; 
			}
		}
	}
	return false;
}

bool find(VPII & s1, int x, int y){
	for(int i=0;i<s1.size();i++){
		if(s1[i].F==x&&s1[i].S==y) return true;
	}
	return false;
}

void solve(int x1,int r1,int x2,int r2, char c1, char c2){
	if(((x1+r1)&1&&!(x2+r2&1))||((x2+r2)&1&&!(x1+r1&1))){ out,"Impossible\n"; return; }
	if(x1==x2&&r1==r2){ out,"0 ",c1," ",r1,NL; return; }
	VPII fromS, fromG;
	generate(fromS, x1, r1);
	if(find(fromS, x2, r2)){ out,"1 ",c1," ",r1," ",c2," ",r2,NL; return; }
	generate(fromG, x2, r2);
	PII its; 
	if(findint(fromS, fromG, its))
		out,"2 ",c1," ",r1," ",(char)(its.F+'A')," ",its.S," ",c2," ",r2,NL;
	else out,"Impossible\n"; // This should never be run with above statement
}

int main(){
#ifdef LOCAL
	freopen("input.1", "r", stdin);
#endif
	char c1,c2;
	int x1,r1,x2,r2;
	for(int t=in;t--;){
		in,c1,r1,c2,r2;
		x1=c1-'A',x2=c2-'A';
		solve(x1,r1,x2,r2,c1,c2);
	}
	return 0;
}
