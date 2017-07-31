#ifndef KNIGHTS_H
#define KNIGHTS_H

#include <utility>
#include <vector>
#include <algorithm>
#include <iostream>

using std::pair;
using std::make_pair;
using std::vector;

typedef vector<pair<int,int> > Path;

/** Helper function: adds two pairs of ints */
pair<int,int> operator+(const pair<int,int> & a, const pair<int,int> & b) {
    return make_pair(a.first + b.first, a.second + b.second);
}

Path moves(pair<int, int> move) {
	Path movers = { make_pair(move.first+1, move.second-2), make_pair(move.first+2, move.second-1), make_pair(move.first+2, move.second+1), make_pair(move.first+1, move.second+2), make_pair(move.first-1, move.second+2), make_pair(move.first-2, move.second+1), make_pair(move.first-2, move.second-1), make_pair(move.first-1, move.second-2) };
	return movers;
}

Path legal_moves(int size, Path path, pair<int, int> move) {
	Path possiblemoves = moves(move);
	Path legalmoves;
	std::copy_if(possiblemoves.begin(), possiblemoves.end(), std::back_inserter(legalmoves), [size, path](const pair<int, int> x) { return ((x.first >= 0) && (x.second >= 0) && (x.first < size) && (x.second < size) && (std::find(path.begin(), path.end(), x) == path.end())); } );
	return legalmoves;
}


pair<Path, bool> first_tour(int size, Path path) {
	Path legalmoves = legal_moves(size, path, path.at(path.size()-1));
	if (path.size() == size*size) {
		return make_pair(path, true);
	}
	else if (legalmoves.size() != 0) {
		for (int i = 0; i < legalmoves.size(); ++i) {
			pair<int, int> move = legalmoves.at(i);
			Path newPath = path;
			newPath.push_back(move);
			pair<Path, bool> first = first_tour(size, newPath);
			if (first.second == true) {
				return first;
			}
		}
	}
	return pair<Path, bool>(Path(), false);
}

// Do not edit below this line

#endif
