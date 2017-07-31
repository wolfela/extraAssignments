#ifndef HOW_MANY_WAYS_TO_MAKE_H
#define HOW_MANY_WAYS_TO_MAKE_H
#include<vector>
using std::vector;

#include<vector>

template<typename T>
int howManyWaysToMake(T begin, T end, int toMake) {
	if (toMake == 0) {
		return 1;
	}	
	else if (toMake < 0 || begin == end) {
		return 0;
	}
	else {
		return howManyWaysToMake(begin, end, toMake-*begin) + howManyWaysToMake(++begin, end, toMake);	
	}

};
// Do not write any code below this line

#endif
