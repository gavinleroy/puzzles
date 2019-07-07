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
static inline T gcdExtended(T a, T b, T *x, T *y) {
    if (a == 0) {
        *x = 0, *y = 1;
        return b;
    }
    T x1, y1;
    T gcd = gcdExtended(b%a, a, &x1, &y1);
    *x = y1 - (b/a) * x1;
    *y = x1;
    return gcd;
}

template <class T>
inline static T modInverse(T b, T m) {
    T x, y;
    T g = gcdExtended(b, m, &x, &y);
    if (g != 1) return -1;
    return (x%m + m) % m;
}

template <class T>
inline static T mDiv(T a, T b, T m) {
    a = a % m;
    T inv = modInverse(b, m);
    if (inv == -1) return -1;
    else return (inv * a) % m;
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

ll ff(ll n, ll MOD){
	ll m = n/2LL, u;
	if(n&1) u = pw(3LL, m, MOD);
	else{
		if(!(m&1)) u = (pw(3LL, m, 2LL*MOD) + 5LL )/2LL;
		else u = (pw(3LL, m, 2LL*MOD) - 1LL )/2LL;
	}
	return u % MOD;
}

ll nsum(ll N, ll MOD){
	if(N==0) return 0;
	ll n = N/4LL - 1LL;
	ll f_sum = 0, s_sum = 0;
	if(n > -1){
		ll t1 = (20LL * pw(9LL, n+1, 16LL*MOD)) % MOD;
		ll t2 = (32LL * n) % MOD;
	       	f_sum =  mDiv((t1 + t2 + 12LL) % MOD, 16LL, MOD);
		if(f_sum == -1)
		       	f_sum =  (20LL * pw(9LL, n+1, 16LL*MOD)  + 32LL * n + 12LL) / 16LL;
	}
	for(ll i = 4LL*n+5LL; i <= N; i++) s_sum += ff(i, MOD);
	return ((f_sum % MOD) + (s_sum % MOD)) % MOD;
}

void solve(ll l, ll r, ll MOD){
	ll ans = (nsum(r, MOD) + MOD - nsum(l - 1,MOD)) % MOD;
	cout << ans%MOD << endl;
}

int main(void){BOOST
	init_time();
	#ifdef LOCAL
		freopen("input.1", "r", stdin);
	#endif
	ll t, MOD, l, r;
	cin >> t >> MOD;
	while(t--){
		cin >> l >> r;
		solve(l, r, MOD);
	}
	print_time("Time: ");
	return 0;
}
