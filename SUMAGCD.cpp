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

int aa[100001];

int findMaxGCD(int arr[], int n, int high) { 
	int divisors[high + 1] = { 0 }; 
	for (int i = 0; i < n; i++) { 
		for (int j = 1; j <= sqrt(arr[i]); j++) { 
			if (arr[i] % j == 0) { 
				divisors[j]++; 
				if (j != arr[i] / j) divisors[arr[i] / j]++; 
			} 
		} 
	} 

	for (int i = high; i >= 1; i--){
		if (divisors[i] > 1) return i;     
	}
} 
int solve(int aa[], int n, int max){
	int g1 = aa[0], g2 = aa[1];
	for(int i = 2; i < n; i++){
		int t1 = gcd(g1, aa[i]), t2 = gcd(g2, aa[i]);
		if(t1-g1 > t2-g2){
//			cout << "Gcd1 was: " << g1 << " will be " << t1 << endl;
//			cout << "added : " << aa[i] << " to arr1" << endl;
		       	g1 = t1;
		}else{
//			cout << "Gcd2 was: " << g2 << " will be " << t2 << endl;
//			cout << "added : " << aa[i] << " to arr2" << endl;
		       	g2 = t2;
		}
	}
	return g1 + g2;
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
		for(int i = 0; i < n; i++){ 
			cin >> aa[i];	
			remax(max, aa[i]);
		}
		cout << solve(aa, n, max) << endl;
	}	
	print_time("Time: ");
	return 0;
}
