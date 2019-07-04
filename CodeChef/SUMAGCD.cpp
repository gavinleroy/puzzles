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

int pp[100001], ss[100001];

int solve(vector<int> vv, int n){
	if(n==1) return vv[0] * 2;

	pp[0] = vv[0];
	ss[n-1] = vv[n-1];
	for(int i = 1; i < n; i++) pp[i] = gcd(pp[i-1], vv[i]);
	for(int i = n-2; i >= 0; i--) ss[i] = gcd(ss[i+1], vv[i]);
	int ans = max(vv[0] + ss[1], pp[n-2] + vv[n-1]);
	for(int i = 1; i < n-1; i++) remax(ans, vv[i] + gcd(pp[i-1], ss[i+1]));
	return ans;
}

int main(void){BOOST
	init_time();
	#ifdef LOCAL
		freopen("input.1", "r", stdin);
	#endif
	int t, n, max;
	cin >> t;
	while(t--){
		cin >> n;
		set<int> ss;
		vector<int> aa;
		for(int i = 0; i < n; i++){
			int temp;
		       	cin >> temp;
			if(ss.insert(temp).S){
				aa.push_back(temp);
			}
		}
		cout << solve(aa, aa.size()) << endl;
	}	
	print_time("Time: ");
	return 0;
}
