#include <iostream>
#include <functional>


#include <vector>
#include <set>
using namespace std;

class CollatzSolver
{
public:
    const static unsigned int MAX_LEN = 1000001;
    unsigned long long collatzLens [MAX_LEN] = {0};
    std::vector<unsigned long long> unCalcIdx;
    unsigned long long maxLength = 0;
    unsigned long long maxIdx = 0;
    
    unsigned long long lengthOf(unsigned long long n)
    {
        if (n ==1) {
            return 1;
        }
        else
        {
            if (n < MAX_LEN && collatzLens[n] != 0) {
                return collatzLens[n];
            }
            else
            {
                if (n %2 == 0) {
                    return  1 + lengthOf(n/2);
                }
                else
                {
                    return 1 + lengthOf(3*n+1);
                }
            }
        }
    }
    void firstPass()
    {
        //first pass
        for (unsigned long long i = 1; i < MAX_LEN; ++i)
        {
            collatzLens[i] = lengthOf(i);
            if (maxLength < collatzLens[i]) {
                maxLength = collatzLens[i];
                maxIdx = i;
            }
        }

    }
    void solve()
    {
        firstPass();
    }
    
};
int  main(int argc, char const *argv[])
{
    CollatzSolver solver;
    solver.solve();
    cout << solver.maxLength << " " << solver.maxIdx << endl;
    return 0;
}