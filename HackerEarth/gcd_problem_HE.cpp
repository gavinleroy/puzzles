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
#define ll int64_t
#define ull uint64_t
#define MOD (int)(1e9+7)

template<typename T>
static inline T gcd(T a, T b) {
    a = std::abs(a);
    b = std::abs(b);
    while (b > 0) {
        a %= b;
        std::swap(a, b);
    }
    return a;
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
 
template <class T>
static inline T pw(T a, ll n) {
    T ret = T(1);
    while (n > 0) {
        if (n & 1) ret = ret * a;
        a = a * a;
        n >>= 1;
    }
    return ret;
}
 
template <class T>
static inline T pw(T a, T n, T mod) {
    T ret = T(1);
    while (n > 0) {
        if (n & 1) ret = ret * a % mod;
        a = a * a % mod;
        n >>= 1;
    }
    return ret;
}

template <class T>
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

int fac[100001], dp[260001];

int modInverse(int n) {
    return pw(n, MOD-2, MOD);
}

void fill_fac(){
	fac[0] = 1;
	for(int i=1 ; i<=100001; i++) fac[i] = fac[i-1]*i%MOD;
}

int ch(int n, int r){
   if (r==0) return 1;
    return (fac[n]* modInverse(fac[r]) % MOD * modInverse(fac[n-r]) % MOD) % MOD;
}

void solve(int n){
	int res = 0;
	int i = (int)n/4;
	while(i >= 1){
		dp[i]=ch((int)n/i, 4);	
		if(i <= (int)n/4){
			for(int j = i*2; j <= (int)n/4; j += i) dp[i] -= dp[j]; 
			i--;		
		}
	}
	for(int i = 1; i <= (int)n/4; i++){
		res = (res + ((((((dp[i] * i) % MOD) * i) % MOD) * i) % MOD) * i) % MOD;
	}
	cout << res << endl;
}

int main(void){BOOST
	init_time();
	#ifdef LOCAL
		freopen("input.1", "r", stdin);
	#endif
	fill_fac();
	int t, n;
	cin >> t;
	while(t--){
		cin >> n;
		solve(n);
	}
	print_time("Time: ");
	return 0;
}
