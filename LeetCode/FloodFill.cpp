class Solution {
private:
    vector<pair<int, int>> dirs = {make_pair(-1, 0), 
                                   make_pair(1, 0), 
                                   make_pair(0, -1), 
                                   make_pair(0, 1)};
public:
    vector<vector<int>> floodFill(vector<vector<int>>& image, int sr, int sc, int newColor) {

	// Create 2d vector initialized to false
        vector<vector<bool>> visited(image.size(), vector<bool>(image[0].size(), false));

	// Remember that things are passed by reference
        floodFillUtil(image, visited, sr, sc, newColor);
        return image;
    }
    
    
    
    void floodFillUtil(vector<vector<int>> & image,
		    vector<vector<bool>> & visited, 
		    int sr, int sc, int newColor) { 

	// Visit this node
        visited[sr][sc] = true;
        for(pair p : dirs){ 

	    // If we can go to the adjacent neighbors
            if(sr + p.first >= 0 && sc + p.second < image[0].size() &&
               sr + p.first < image.size() && sc + p.second >= 0 &&
               !visited[sr+p.first][sc+p.second] && 
               image[sr+p.first][sc+p.second] == image[sr][sc]){

		// DFS to the next nodes
                floodFillUtil(image, visited, sr+p.first, sc+p.second, newColor);
            }
        }
	// Alter the value of this node after the fact, when leaving the recursion
        image[sr][sc] = newColor;
    }
};
