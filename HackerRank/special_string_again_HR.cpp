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

void swpp(int* a, int* b){
	for(int i = 0; i < 27; i++){
		swap(a[i], b[i]);
	}
}

int eval_pal(int** aa){
	if(aa[2][0] == aa[0][0] && aa[1][aa[1][0]] == 1) return min(aa[2][aa[2][0]], aa[0][aa[0][0]]);
	return 0;
}

/* I made this solution more complicated than necessary, and it really didn't optimize any further
 * than a slightly simpler solution would have been.
 *
 * A simipler solution would be to make a vector of std::pair<int, char> where the int represents the number of
 * chars found sequentially. Then to go over this vector and compute the answer. 
 * This would simplify the look of the code and shorten it as well.
 * 
 * My solution below has a 2D array of int. Where the zeroeth element of each row holds the char, and the 
 * remaining spaces hold the count for that particular char. This made things slightly overcomplicated. Originally
 * I thought this idea may save a little on space and would optimize time complexity, however, it was 
 * overly complicated for how little it actually would have saved.
 *
 * Lesson learned.
 * */
int solve(int n, string s){
	int ans = 0;
	int** aa = new int*[3];
	for(int i = 0; i < 3; i++){ 
		aa[i] = new int[27];
		for(int k = 0; k <= 26; k++) aa[i][k] = 0;
	}
	aa[0][0] = aa[1][0] = aa[2][0] = -1;
	int i = 0;
	//This fist for loop sets the matrix witht he inital values. Could be simplified.
	//However it's not worth my time to refactor the code
	for(int j = 0; j < 3; j++){
		if(i < n){
		       	aa[j][0] = s[i++] - 97 + 1;
			aa[j][aa[j][0]]++;
		}else break;
		while(i < n && s[i]-97 + 1 == aa[j][0]){
			i++;
			aa[j][aa[j][0]]++;
		}
		if(j < 2) ans += ((aa[j][aa[j][0]] * (aa[j][aa[j][0]]+1)) / 2);
	}
	
	//This while loop takes care of all theh following sequences after the inital three.
	while(i <= n){
		if(i < n && s[i]-97+1 != aa[2][0]){
			ans += eval_pal(aa);
			ans += ((aa[2][aa[2][0]] * (aa[2][aa[2][0]]+1)) / 2);
			swpp(aa[0], aa[1]);
			swpp(aa[1], aa[2]);
			aa[2][aa[2][0]] = 0;
			aa[2][0] = s[i++] - 97 + 1;
			aa[2][aa[2][0]]++;
		}else if(i == n){
			ans += eval_pal(aa);
			ans += ((aa[2][aa[2][0]] * (aa[2][aa[2][0]]+1)) / 2);
			i++;
		}else{
			i++;
			aa[2][aa[2][0]]++;
		}
	}
	for(int k = 0; k < 3; k++) delete []aa[k];
	delete []aa;
	return ans;
}

int main(void){BOOST
	init_time();
	#ifdef LOCAL
		freopen("input.1", "r", stdin);
	#endif
	int n;
	cin >> n;
	string s;
	cin >> s;
	cout << solve(n, s) << endl;
	print_time("Time: ");
	return 0;
}
