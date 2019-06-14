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
//#define MOD 1000000007

typedef long long ll;
typedef unsigned long long ull;
const ll MOD = 1000000007;

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
 
static inline ll pw(ll a, ll n, ll mod) {
    ll ret = 1;
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
        if ((b & 1) == 0) 
            res = (res + a) % mod; 
        a = (a << 1) % mod; 
        b >>= 1; 
    } 
    return res % mod; 
} 

using namespace std;

ll solve(ll k, ll n){
	ll ret = (k-1LL) % MOD;
	ll i = ((k-n)/(n-1LL)) + (((k-n)%(n-1LL) == 0LL) ? 0LL: 1LL);	
	if(k > n){
		ll f = -1LL*(n - 1LL);
		ll s = (i*(i+1LL))/2LL;
		ll l = i*(k-1LL);
//		cout << "f: " << f << " s: " << s << " l: " << l << endl;
//		cout << "ret_before: " << ret;
		ret = (ret + ((f*s) + l) % MOD) % MOD;
//		cout << " after: " << ret << endl;
//		cout << "adding: " << ((mul(f,s, MOD) % MOD) + l) % MOD << endl;
//		cout << "f*s: " << mmm(f, s, MOD) << " l: " << l << endl;
	}
	return ret % MOD;
}

ll solve_2(ll k, ll n){
	ll ret = k-1, t = 0;
	ll i = 1;
	while(k > n){
		ll temp = (k*(i+1)) - ((k+n-1)*i) - 1;
		if(temp <= 0) break;
		t = (t + (ll)temp) % MOD;
		i++;
	}
	return (ret + t) % MOD;
}

int main(void){BOOST
	init_time();
	#ifdef LOCAL
		freopen("input.1", "r", stdin);
	#endif
	int t;
	ll n, k;
//	cin >> t;
//	while(t--){
//		cin >> n >> k;
//		cout << solve(k, n) << endl;
//	}
	for(ll i = 7373633; i > 100000; i--){
		for(ll j = 139029299; j > 2; j--){
			ll ans = solve(i, j);
			ll ans_1 = solve_2(i, j);
			if(ans != ans_1){ 
				cout << "Broke (k, n)-> " << i << " " << j << endl;
				cout << "Optimized: " << ans << " Non: " << ans_1 << endl;
				return 0;
			}
		}
	}
	print_time("Time: ");
	return 0;
}
