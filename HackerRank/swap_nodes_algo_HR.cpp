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
static inline T ext(T a, T b, T *x, T *y) {
    if (a == 0) {
        *x = 0, *y = 1;
        return b;
    }
    T x1, y1;
    T gcd = ext(b%a, a, &x1, &y1);
    *x = y1 - (b/a) * x1;
    *y = x1;
    return gcd;
}

template <class T>
inline static T mInv(T b, T m) {
    T x, y;
    T g = ext(b, m, &x, &y);
    if (g != 1) return -1;
    return (x%m + m) % m;
}

template <class T>
inline static T div(T a, T b, T m) {
    a = a % m;
    T inv = mInv(b, m);
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

struct node{
	int l;
	int r;
	int h;
};

void set_height(node* aa, int r, int h){
	if(r == -1) return;
	aa[r].h = h;
	set_height(aa, aa[r].l, h+1);
	set_height(aa, aa[r].r, h+1);	
}

string p_swaps(node* aa, int r, int k){
	if(r == -1) return "";
	string s =  "";
	if(aa[r].h % k == 0) swap(aa[r].l, aa[r].r);
	s += p_swaps(aa, aa[r].l, k);
	s += (to_string(r) + ' ');
	s +=  p_swaps(aa, aa[r].r, k);
	return s;
}

void solve(node* aa, int t){
	while(t--){
		int k;
		cin >> k;
		cout << (p_swaps(aa, 1, k) + "\n");
	}
}

int main(void){BOOST
	init_time();
	#ifdef LOCAL
		freopen("input.1", "r", stdin);
	#endif
	int n, t;
	cin >> n;
	node* aa = new node[n+1];
	for(int i = 1; i <= n; i++) cin >> aa[i].l >> aa[i].r;
	cin >> t;
	set_height(aa, 1, 1);
	solve(aa, t);
	print_time("Time: ");
	return 0;
}
