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
#include <cstring>
#include <utility>
#include <algorithm>
#include <vector>
#define NL '\n'
#define PII pair<int,int>
#define VPII pair<PII >
#define MP(x,y) make_pair(x,y)
#define F first
#define S second

using namespace std;

int n,aaa[26],bbb[26];
char aa[1001],bb[1001];

void solve(){
	bool done[26]={};
	vector<vector<int> >ans;
	vector<pair<char,char> > ss;	
	for(int i=0;i<n;i++) ss.push_back(MP(aa[i],bb[i]));
	sort(ss.begin(), ss.end(), 
			[](const pair<char,char> & lhs, const pair<char,char> & rhs){
				return lhs.S > rhs.S;	
			});
	for(int i=0;i<n;i++){
		if(ss[i].F!=ss[i].S && !done[ss[i].S-'a']){
			vector<int> vv;
			for(int j=0;j<n;j++)
				if(aa[j]==ss[i].S||bb[j]==ss[i].S)
					aa[j]=ss[i].S,vv.push_back(j);
			sort(vv.begin(), vv.end());
			ans.push_back(vv);
			done[ss[i].S-'a']=true;
		}
	}
	// PRINT OUT THE ANSWER
	out,ans.size(),NL;
	for(int i=0;i<ans.size();i++){
		out,ans[i].size(),' ';
		for(int j=0;j<ans[i].size();j++)out,ans[i][j],' ';
		out,NL;
	}
}

int main(){
#ifdef LOCAL
	freopen("input.1", "r", stdin);
#endif
	for(int T=in;T--;){
		in,n,aa,bb;
		memset(aaa, 0, sizeof aaa);
		memset(bbb, 0, sizeof bbb);
		bool CAN=true;
		for(int i=0;i<n;i++) aaa[aa[i]-'a']++;
		for(int i=0;i<n;i++){ bbb[bb[i]-'a']++; if(bb[i]>aa[i])CAN=false; }
		for(int i=0;i<26;i++)if(bbb[i]&&!aaa[i])CAN=false;
		if(CAN) solve();
		else out,-1,NL;
	}
	return 0;
}
