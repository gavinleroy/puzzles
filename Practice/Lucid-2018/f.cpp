#include <iostream>
using namespace std;

void lol(void)
{
	int p, m, n;
	cin >> p >> m >> n;
	int foo = m + n;
	bool isJacob = true;
	if( !((p >= m) && (p <= n)) )
	{
		cout << "Jacob\n";
		isJacob = false;
	}
	else
	{
		for(int i = m; i <= n; i++)
		{
			if((p / i) % 2 == 0)
			{
				isJacob = false;
			}
		}	
	}
	if(isJacob)
	{
		cout << "Jacob\n";
	}
	else
	{
		cout << "Megan\n";
	}
}

int main()
{
	int i;
	cin >> i;
	for(int j = 0; j < i; j++)
	{
		lol();
	}
	return 0;
}
