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
	int v;
	int m;
};

void zero(node* aa, int i, int l, int r){
	aa[i].v = 0;
	aa[i].l = l;
	aa[i].r = r;
	aa[i].m = 0;
	if(l == r) return;
	int mid = (r + l) / 2;	
	zero(aa, i*2, l, mid);
	zero(aa, i*2 + 1, mid+1, r);
}

int uN(node* aa, int* ll, int i, int k){
//	cout << "Set on node with range [" << aa[i].l << ", " << aa[i].r << "]\n";
	aa[i].v += k;
	aa[i].v += ll[i];
	ll[i*2] += ll[i]+k;
	ll[i*2 + 1] += ll[i]+k;
	ll[i] = 0;
	remax(aa[i].m, aa[i].v);
	return aa[i].m;
}

int update(node* aa, int* ll, int i, int l, int r, int k){
//	cout << "range in question: [" << aa[i].l << ", " << aa[i].r << "]\n";
	if(l <= aa[i].l && aa[i].r <= r) return uN(aa, ll, i, k);
	else if(aa[i].r < l || aa[i].l > r) return -1;
	else return max(aa[i].m, max(update(aa, ll, i*2, l, r, k), update(aa, ll, i*2 + 1, l, r, k)));	
}

int f_update(node* aa, int* ll, int i){
	if(aa[i].l >= aa[i].r) return aa[i].v + ll[i];
	else{
		aa[i].v += ll[i];
		ll[i*2] += ll[i];
		ll[i*2 + 1] += ll[i];
		ll[i] = 0;
		return max(f_update(aa, ll, i*2), f_update(aa, ll, i*2 + 1));
	}
}

void print(node* aa, int i){
	if(aa[i].l >= aa[i].r) cout << aa[i].v << " ";
	else{ print(aa, i*2); print(aa, i*2 + 1);}
}

void solve(node* aa, int* ll, int n, int m){
	zero(aa, 1, 1, n);
	for(int i = 0; i < n*3 + 1; i++) ll[i] = 0;

	int ans = 0;
	while(m--){
		int l, r, k;
		cin >> l >> r >> k;
//		print(aa, 1);
//		cout << endl;
		ans = update(aa, ll, 1, l, r, k);
	}
	ans = f_update(aa, ll, 1);
//	print(aa, 1); cout << endl;
	cout << ans << endl;
}

int main(void){BOOST
	init_time();
	#ifdef LOCAL
		freopen("input.1", "r", stdin);
	#endif
	int n, m;
	cin >> n >> m;
	node* aa = new node[n*3 + 1];
	int* ll = new int[n*3 + 1];
	solve(aa, ll, n, m);
	delete []ll;
	delete []aa;
	print_time("Time: ");
	return 0;
}
