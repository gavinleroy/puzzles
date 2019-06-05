#include <iostream>
#include <string>

using namespace std;

int main(void){
	int p;
	cin >> p;
	bool* aa = new bool[26];
	while(p--){
		bool ans = false;
		for(int i = 0; i < 26; i++) aa[i] = false;
		string s1, s2;
		cin >> s1 >> s2;
		for(int i = 0; i < s1.size(); i++){
			aa[s1[i] - 97] = true;
		}
		for(int i = 0; i < s2.size() && !ans; i++){
			ans = ans || aa[s2[i]-97];
		}
		cout << ((ans) ? "YES": "NO") << endl;
	}
	delete []aa;
	return 0;
}
