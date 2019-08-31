/* Hidden stub code will pass a root argument to the function below. Complete the function to solve the challenge. Hint: you may want to write one or more helper functions.  

The Node struct is defined as follows:
	struct Node {
		int data;
		Node* left;
		Node* right;
	}
*/

//This problem does not offer the rest of the code.
//	therefore you just need to go to the site and
//	copy/paste this into the editor. 
//
//	https://www.hackerrank.com/challenges/ctci-is-binary-search-tree/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=trees
    int aa[100001];

    bool check(Node* root, int min, int max){
        if(root == nullptr) true;
        else{
            if(aa[root->data] != 0) return false;
            
            aa[root->data] = 1;
            if(root->data <= min || root->data >= max) return false;
            else return check(root->left, min, root->data) && check(root->right, root->data, max);
        }
        return true;
    }    

	bool checkBST(Node* root) {
        fill_n(aa, 100001, 0);
        int MIN = (1 << 31);
        int MAX = ~(1 << 31);
		return check(root, MIN, MAX);
	}
