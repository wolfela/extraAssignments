#ifndef TRADE_H
#define TRADE_H
#include <algorithm>
using std::transform;
using std::max_element;

template <typename T>
int bestProfit(T first, T last) {
	int minimum = *first;
	transform(first, last, first, [&minimum](const int & x) { if (x < minimum) minimum = x; return x - minimum; });
	T maxProfit = max_element(first, last, [](const int & x, const int & y) { return (x < y); });
	return *maxProfit;
}

// Don't write any code below this line

#endif
