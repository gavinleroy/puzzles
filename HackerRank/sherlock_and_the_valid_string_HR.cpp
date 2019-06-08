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

using namespace std;

bool solve(string s){
	bool ret = true, d = false, _d = false;
	int* aa = new int[26];
	for(int i = 0; i < 26; i++) aa[i] = 0;
	for(int i = 0; i < s.size(); i++) aa[s[i] - 97]++;
	sort(aa, aa+26, greater<int>());
	int i1 = aa[0], f1 = 1, i2 = -1, f2 = 0, ind = 1; 
	while(ind < 26 && aa[ind] != 0){
		if(aa[ind] == i1) f1++;
		else if(aa[ind] != i1 && i2 == -1){ i2 = aa[ind]; f2++; }
		else if(aa[ind] == i2 && f1 == 1 && abs(i1 - i2) == 1) f2++;
		else return false;
		ind++;
	}
	if(i2 != -1 && abs(i1 - i2) > 1 && min(i1, i2) != 1) ret = false; 
	delete []aa;
	return ret;
}

int main(void){BOOST
	init_time();
	#ifdef LOCAL
		freopen("input.1", "r", stdin);
	#endif
	string s;
	cin >> s;
	cout << ((solve(s)) ? "YES": "NO") << endl; 

	print_time("Time: ");
	return 0;
}
