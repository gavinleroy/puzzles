// PROBLEM: https://www.hackerrank.com/challenges/recursive-digit-sum/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=recursion-backtracking
#include <vector>
#include <iostream>
#include <set>
#include <unordered_set>
#include <map>
#include <unordered_map>
#include <stack>
#include <queue>
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

int sum_string(string n, int s, int e){
	if(s >= e) return 0;
	else if(e-s == 1) return n[s]-48;
	else{
		string str = to_string(sum_string(n, s, (e+s)/2) + sum_string(n, (e+s)/2, e));
		if(str.size() == 1) return str[0]-48;
		else return sum_string(str, 0, str.size());
	}
}

//Note that solve was my original recursive solution.
//	it does provide the correct output, and
//	on small input cases it actually terminated
//	quicker than q_solve. However, on large inputs
//	the recursive solution is too slow and takes about 
//	1.3 seconds for the largest input possible given the 
//	restraints of the problem.
void solve(string n, int k){
	int sum = sum_string(n, 0, n.size()) * k;
	string sum_s = to_string(sum);
	cout << sum_string(sum_s, 0, sum_s.size()) << endl;
}

int digi_sum(string n){
	int ans = 0;
	for(int i = 0; i < n.size(); i++) ans = (ans + (n[i]-'0'))%9;
	return ans%9;
}

void q_solve(string n, int k){
	if(k%9 == 0){
		cout << 9 << endl;
	}else{
		int si = (digi_sum(n)*k)%9;
		cout << ((si == 0) ? 9: si) << endl;
	}
}

// The solved problem can be found here-> https://www.hackerrank.com/challenges/recursive-digit-sum/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=recursion-backtracking
int main(void){BOOST
	init_time();
	#ifdef LOCAL
		freopen("input.1", "r", stdin);
	#endif
	string n;
	int k;
	cin >> n;
	cin >> k;
	q_solve(n, k);
	print_time("Time: ");
//	To test the timing difference between q_solve and solve.
//		as stated above solve is faster for small inputs,
//		and q_solve is faster for large inputs.
//		They both provide correct output always.
//		Solve was my initial solution because I did not 
//		think of the mathematical optimization that is
//		used in q_solve.
//	init_time();
//	solve(n, k);
//	print_time("Time: ");
	return 0;
}
