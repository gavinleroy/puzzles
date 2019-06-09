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

static inline ll ncr(ll n, ll r) {
    if (r == 0) return 1;
    if (r > n / 2) return ncr(n, n - r); 

    long res = 1; 

    for (ll k = 1; k <= r; ++k) {
        res *= n - k + 1;
        res /= k;
    }
    return res;
}

using namespace std;

ll solve(double* aa, int n, double r){
	vector<int>* pp = new vector<int>[50];
	ll ret = 0;
	for(int i = 0; i < n; i++){
		double temp;
		if((int)aa[i] == 1) pp[0].push_back(i);
		if(aa[i] >= r) temp = log(aa[i])/log(r);	
		else continue;
		if(pw(r, (int)temp) == aa[i]) pp[(int)temp].push_back(i);
	}
	for(int i = 0; i < 48; i++){
		int ind_1 = 0, ind_2 = 0;
		cout << pp[i].size() << endl;
		for(int j = 0; j < pp[i].size(); j++){			
			while(ind_1 < pp[i+1].size() && pp[i+1][ind_1] < pp[i][j]) ind_1++;
			while(ind_2 < pp[i+2].size() && pp[i+2][ind_2] < pp[i][j]) ind_2++;
			ret += (ll)((ll)(pp[i+1].size() - ind_1) * (ll)(pp[i+2].size() - ind_2));
		}
	}
	delete []pp;
	return ret;
}


ll solve_1(ll n){
	return ncr(n, 3);
}

int main(void){BOOST
	init_time();
	#ifdef LOCAL
		freopen("input.1", "r", stdin);
	#endif
	int n;
	double r;
	cin >> n >> r;
	double* aa = new double[n];
	for(int i = 0; i < n; i++) cin >> aa[i];
	cout << ((r == 1) ? solve_1(n) : solve(aa, n, r)) << endl;
	delete []aa;
	print_time("Time: ");
	return 0;
}
