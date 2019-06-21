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

double cal_med(int* cc, int d){
	if(!(d&1)) return (double)(((double)cc[d/2 - 1] + (double)cc[d/2])/2.0);
	else return cc[d/2]; 
}

void build_c(int* ff, int* cc, int d){
	int i = 0;
	for(int j = 0; j < 201 && i < d; j++){
		int temp = ff[j];
		while(temp--) cc[i++] = j;
	}
}

int solve(int* aa, int* ff, int n, int d){
	for(int i = 0; i < 201; i++) ff[i] = 0;
	for(int i = 0; i < d; i++){
		cin >> aa[i];
		ff[aa[i]]++;
	}
	int* cc = new int[d];
	build_c(ff, cc, d);
	int ans = 0;
	double med;
	for(int i = d; i < n; i++){
		med = cal_med(cc, d);
		cin >> aa[i];
//		cout << "med: " << med << " aa[i] " << aa[i] << endl;
		if(2.0*med <= aa[i]) ans++;
		ff[aa[i]]++;
		ff[aa[i-d]]--;
		if(aa[i] != aa[i-d]) build_c(ff,cc, d);
	}
	return ans;
}

int main(void){BOOST
	init_time();
	#ifdef LOCAL
		freopen("input.1", "r", stdin);
	#endif
	int n; 
	int d;
	int* aa = new int[200001];
	int* ff = new int[201];
	cin >> n >> d;	
	cout << solve(aa, ff, n, d) << endl;
	delete []aa;
	delete []ff;
	print_time("Time: ");
	return 0;
}
