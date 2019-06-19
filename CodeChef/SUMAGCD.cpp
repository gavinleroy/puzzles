#include <vector>
#include <iostream>
#include <set>
#include <unordered_set>
#include <map>
#include <unordered_map>
#include <iomanip>
#include <random>
#include <functional>
#include <cassert>
#include <cstdio>
#include <cinttypes>
#include <cstdlib>
#include <cstdint>
#include <utility>
#include <cstring>
#include <climits>
#include <algorithm>
#include <forward_list>
#include <thread>

#define BOOST std::ios::sync_with_stdio(0); cin.tie(NULL); cout.tie(NULL);
#define P std::pair<int, int>
#define F first
#define S second

#define MAX_INT ~(1 << 31)
#define MIN_INT (1 << 31)
#define MAX_LL ~(1 << 63)
#define MIN_LL (1 << 63)

typedef long long ll;
typedef unsigned long long ull;

template<typename T>
static inline T gcd(T a, T b) {
    if(a == -1) return b;
    if(b == -1) return a;
    a = std::abs(a);
    b = std::abs(b);
    while (b > 0) {
        a %= b;
        std::swap(a, b);
    }
    return a;
}

template<typename T>
static inline T abs(T a, T b){
	return ((a-b < 0) ? b-a: a-b);
}
 
template<typename T>
static inline T sqr(T a) {
    return a * a;
}
 
template<typename T>
static inline bool remin(T &a, const T &val) {
    if (a > val) {
        a = val;
        return true;
    }
    return false;
}
 
template<typename T>
static inline bool remax(T &a, const T &val) {
    if (a < val) {
        a = val;
        return true;
    }
    return false;
}

static double clock_t0;
 
static inline void init_time() {
    clock_t0 = clock();
} 

static inline double get_time() {
    return (clock() - clock_t0) / CLOCKS_PER_SEC;
}
 
static inline void print_time(const std::string &name) {
    std::cerr << name << " " << get_time() << "s." << std::endl;
}
 
template<typename T>
static inline T pw(T a, ll n) {
    T ret = T(1);
    while (n > 0) {
        if (n & 1) ret = ret * a;
        a = a * a;
        n >>= 1;
    }
    return ret;
}
 
template<typename T>
static inline T pw(T a, T n, T mod) {
    T ret = T(1);
    while (n > 0) {
        if (n & 1) ret = ret * a % mod;
        a = a * a % mod;
        n >>= 1;
    }
    return ret;
}

template<typename T>
inline static T mul(T a, T b, T mod) { 
    T res = T(0); 
    a = a % mod; 
    while (b > 0){ 
        if (b % 2 == 1) 
            res = (res + a) % mod; 
        a = (a * 2) % mod; 
        b >>= 1; 
    } 
    return res % mod; 
} 

using namespace std;

const int _MAX = 100001, __MAX = 1000000001;

int aa[_MAX];

static inline void p_max(pair<int, int> &p, int f, int s){
	if(s+f > p.F+p.S) p = make_pair(f,s);
}

pair<int, int> solve(int s, int e){
	if(e-s == 2) return make_pair(aa[s], aa[s+1]);
	else if(e-s == 1) return make_pair(aa[s], -1);
	else if(e-s == 0) return make_pair(-1, -1);
	else if(e-s < 0) throw "Error occured";

	int m = (e-s)/2;
	pair<int, int> p1 = solve(s, s+m), p2 = solve(s+m, e), p3 = make_pair(1,1);	
	int p11 = gcd(p1.F, p1.S), p22 = gcd(p2.F, p2.S); int p12 = gcd(p1.F, p2.F), p21 = gcd(p1.S, p2.S);
	int p112 = gcd(p1.F, p2.S), p221 = gcd(p1.S, p2.F);
	if(p1.F != -1) p_max(p3, p1.F, gcd(p1.S, p22));		
	if(p1.S != -1) p_max(p3, p1.S, gcd(p1.F, p22));
	if(p2.F != -1) p_max(p3, p2.F, gcd(p2.S, p11));		
	if(p2.S != -1) p_max(p3, p2.S, gcd(p2.F, p11));		
	if(p21 != -1 && p12 != -1) p_max(p3, p12, p21);		
	if(p112 != -1 && p221 != -1) p_max(p3, p112, p221);		
	return p3;
}

//int solve(int n){
//	pair<int, int> p = solve(0, n);
//	return p.F+p.S; 
//}

int solve(int n){
	int high = aa[0];
	int g = aa[1];
	for(int i = 2; i < n; i++){
		if(aa[i] != high) g = gcd(g, aa[i]);
	}
	return high + g;
}

int main(void){BOOST
	init_time();
	#ifdef LOCAL
		freopen("input.1", "r", stdin);
	#endif
	int t, n, max;
	cin >> t;
	while(t--){
		max = 0;
		cin >> n;
		for(int i = 0; i < n; i++) cin >> aa[i];	
		sort(aa, aa+n, greater<int>());
		cout << solve(n) << endl;
	}	
	print_time("Time: ");
	return 0;
}
