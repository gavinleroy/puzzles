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
#define NL '\n'
#define PII pair<int,int>
#define MP(x,y) make_pair(x,y)
#define F first
#define S second

using namespace std;

ll x,y,l,r,m,h,pre;
#define f(z) ((x&z)*(y&z))

int prefix(){
	pre=0ll;
	for(int i=62;i>=0;i--){
		ll t=(1ll<<i);
		if((l&t)==(r&t)){
			if(r&t) pre|=t;
		} else return i;
	}
	return 0;
}

void check(ll ts){
	if(l<=ts&&ts<=r){
		ll th = f(ts);
		if(h<th) m=ts,h=th;
		else if(h==th)m=::min(m,ts);
	}
}

// Y is the max, X is the min
void solve(){
	int k=prefix();
	ll oz=(x|y),ts;
	check(r); check(l);
	// Try taking as many values of R as possible, then filling the rest
	// of the bits with X|Y. Note, that the ith bit is CLEARED.
	for(int i=k;i>=0;i--){
		ts=pre; // Initialize with prefix
		// Flip all bits in range [k->i+1]DESC to match R
		for(int j=k;j>=i+1;j--) if(r&(1ll<<j)) ts|=(1ll<<j);
		ts&=(~(1ll<<i)); // Clear ith bit
		// Fill remaining bits with optimal Z=X|Y
		for(int j=i-1;j>=0;j--) if(oz&(1ll<<j)) ts|=(1ll<<j);
		check(ts);
	}
	// Try taking as many values of L as possible, then filling the rest
	// of the bits with X|Y. Note, that the ith bit is SET.
	for(int i=k;i>=0;i--){
		ts=pre; // Initialize with prefix
		// Flip all bits in range [k->i+1]DESC to match R
		for(int j=k;j>=i+1;j--) if(l&(1ll<<j)) ts|=(1ll<<j);
		ts|=(1ll<<i); // Set ith bit
		// Fill remaining bits with optimal Z=X|Y
		for(int j=i-1;j>=0;j--) if(oz&(1ll<<j)) ts|=(1ll<<j);
		check(ts);
	}
	out,m,NL;
}

int main(){
#ifdef LOCAL
	freopen("input.1", "r", stdin);
#endif
	for(int T=in;T--;){
		in,x,y,l,r; h=-1,m=~(1ll<<63);
		solve();
	}
	return 0;
}
