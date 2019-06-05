#include <iostream>
#include <unordered_set>
#include <string>

using namespace std;

int main(void){
	int m, n;
	bool ans = true;
	cin >> m >> n;
	unordered_multiset<string> mm;
	for(int i = 0; i < m; i++){
		string temp;
		cin >> temp;
		mm.insert(temp);
	}
	for(int i = 0; i < n; i++){
		string temp;
		cin >> temp;
		auto t = mm.find(temp);
		if(t  == mm.end()){ ans = false; break; }
		else mm.erase(t);
	}
	cout << ((ans) ? "Yes": "No") << endl;
	return 0;
}
