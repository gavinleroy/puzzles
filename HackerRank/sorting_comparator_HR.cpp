#include <iostream>
#include <vector>
#include<algorithm>
using namespace std;

struct Player {
    int score;
    string name;
};

class Checker{
public:
  	// complete this method
    static int comparator(Player a, Player b)  {
        if(a.score > b.score) return 10;
        else if(a.score == b.score && a.name < b.name) return 10;
        else return -1;
    }
};

bool compare(Player a, Player b) {
    if(Checker::comparator(a,b) == -1)
        return false;
    return true;
}

auto cmp = [](const Player a, const Player b){
	if(a.score > b.score) return true;
	else if(a.score == b.score && a.name < b.name) return true;
	else return false;
};

int main()
{
    int n;
    cin >> n;
    vector< Player > players;
    string name;
    int score;
    for(int i = 0; i < n; i++){
        cin >> name >> score;
        Player player;
        player.name = name, player.score = score;
        players.push_back(player);
    }
//    sort(players.begin(), players.end(), cmp);
    sort(players.begin(), players.end(), compare);
    for(int i = 0; i < players.size(); i++) {
        cout << players[i].name << " " << players[i].score << endl;
    }
    return 0;
}
