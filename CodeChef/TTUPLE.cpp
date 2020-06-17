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
#include <cassert>
#include <cstdlib>
#define NL '\n'
#define PII pair<int,int>
#define VPII vector<PII >
#define MP(x,y) make_pair(x,y)
#define F first
#define S second

using namespace std;

int a,b,c,x,y,z;

// Solve linear equation with 2 uknowns, but don't test on a third
bool MAD(vector<int> ii, vector<int> tt){
	int det = ii[0] - ii[1];
	if(det != 0){
		int c1 = (tt[0] - tt[1]) / det, c2 = (ii[0]*tt[1] - tt[0]*ii[1]) / det;
		return ii[0]*c1 + c2 == tt[0] && ii[1]*c1 + c2 == tt[1];
	}else return false;
}

// Solve linear equation for 2 unknowns and test on the third equation
int MAD(){
	int det = a - b;
	if(det != 0){
		int c1 = (x - y) / det, c2 = (a*y - x*b) / det;
		return (c*c1 + c2 == z && a*c1 + c2 == x && b*c1 + c2 == y) ? (c1!=1) + (c2!=0) : 3;
	}else return 3;
}

// Test if the current ii and tt values can be solved in one operation
bool quicktest(vector<int> ii, vector<int> tt){
	if(ii.size() == 0) return false;
	if(ii.size()==1) return true;
	bool can = true;
	int diff = !ii[0] ? 0 : tt[0]/ii[0];
	for(int i=0;i<ii.size();i++) if(ii[i]*diff != tt[i]) can=false;

	if(can) return true;

	can = true;
	diff = tt[0] - ii[0];
	for(int i=0;i<ii.size();i++) if(tt[i]-ii[i] != diff) can=false;
	return can;
}

bool TEST2(vector<int> ii, vector<int> tt){
	if(ii.size() == 2) return true; // We now know that we need all 3 numbers
	// Check if two of the differences is the same
	
	int diff1=tt[0]-ii[0],diff2=tt[1]-ii[1],diff3=tt[2]-ii[2];
	if(diff1 == diff2 || diff1 == diff3 || diff2 == diff3 || 
	   diff1 == diff2+diff3 || diff2 == diff1+diff3 || diff3 == diff1+diff2) 
		return true;

	// Check if two of their magnitudes are the same
	diff1=ii[0]==0 ? 0 : tt[0]/ii[0], diff2=ii[1]==0 ? 0 : tt[1]/ii[1], diff3=ii[2]==0 ? 0 : tt[2]/ii[2];
	if(diff1 == diff2) if(ii[0]*diff1==tt[0] && ii[1]*diff2==tt[1]) return true; 
	if(diff1 == diff3) if(ii[0]*diff1==tt[0] && ii[2]*diff3==tt[2]) return true; 
	if(diff2 == diff3) if(ii[1]*diff2==tt[1] && ii[2]*diff3==tt[2]) return true; 
	// Check if one magnitude is equal to the product of the other two
	if(diff1 == diff2 * diff3 || diff2 == diff1 * diff3 || diff3 == diff1 * diff2)
		if(ii[0]*diff1==tt[0]&&ii[1]*diff2==tt[1]&&ii[2]*diff3==tt[2]) return true; 

	bool ans = false;
	// Make the ith number correct and do add operation to all three options
	for(int i=0;i<3;i++){
		int d=tt[i]-ii[i];
		ans = ans ||
		quicktest(vector<int>({ii[(i+1)%3] + d, ii[(i+2)%3] + d}), 
				vector<int>({tt[(i+1)%3], tt[(i+2)%3]})) || 
		quicktest(vector<int>({ii[(i+1)%3] + d, ii[(i+2)%3]}), 
				vector<int>({tt[(i+1)%3], tt[(i+2)%3]})) || 
		quicktest(vector<int>({ii[(i+1)%3], ii[(i+2)%3] + d}), 
				vector<int>({tt[(i+1)%3], tt[(i+2)%3]})); 
	}
	// Make the ith number correct and do mult operation to all three options
	for(int i=0;i<3;i++){
		int d = ii[i]==0 ? 0 : tt[i] / ii[i];
		if(ii[i] * d == tt[i]) ans = ans ||
		quicktest(vector<int>({ii[(i+1)%3] * d, ii[(i+2)%3] * d}), 
				vector<int>({tt[(i+1)%3], tt[(i+2)%3]})) || 
		quicktest(vector<int>({ii[(i+1)%3] * d, ii[(i+2)%3]}), 
				vector<int>({tt[(i+1)%3], tt[(i+2)%3]})) || 
		quicktest(vector<int>({ii[(i+1)%3], ii[(i+2)%3] * d}), 
				vector<int>({tt[(i+1)%3], tt[(i+2)%3]})); 
	}
	// Make decrease all goals by ith diff and linearly solve with MAD;
	for(int i=0;i<3;i++){
		int d = tt[i] - ii[i];
		ans = ans ||
		quicktest(vector<int>({ii[(i+1)%3], ii[(i+2)%3]}), 
				vector<int>({tt[(i+1)%3] - d, tt[(i+2)%3] - d})) || 
		quicktest(vector<int>({ii[(i+1)%3], ii[(i+2)%3]}), 
				vector<int>({tt[(i+1)%3] - d, tt[(i+2)%3]})) || 
		quicktest(vector<int>({ii[(i+1)%3], ii[(i+2)%3]}), 
				vector<int>({tt[(i+1)%3], tt[(i+2)%3] - d})); 
	}
	return ans;
}

int main(){
#ifdef LOCAL
	freopen("input.1", "r", stdin);
#endif
	vector<int> ii,tt;
	for(int T=in;T--;){
		in,a,b,c,x,y,z;		
		ii.clear(); tt.clear();
		if(a!=x) ii.push_back(a),tt.push_back(x);
		if(b!=y && !(b==a && y==x)) ii.push_back(b),tt.push_back(y);
		if(c!=z && !(c==b && z== y) && !(c==a && z==x)) ii.push_back(c),tt.push_back(z);
		int mad = MAD();
		if(mad < 3) out,mad,NL;
		else if(quicktest(ii, tt)) out,1,NL;
		else if(TEST2(ii, tt)) out,2,NL;
		else out,3,NL;
	}
	return 0;
}
