#include <bits/stdc++.h>
using namespace std;


void solve()
{
    int n,k;
    cin>>n>>k;
    vector<int> v(n);
    for(int i=0;i<n;i++)
    {
        cin>>v[i];
        cout<<i<<endl;
    }
    int best_diff = -1, best_i, best_j;
    int j = -1;
    int curr_sum = 0;
    for(int i=0;i<n;i++)
    {
        while(j + 1 < n && curr_sum <= k)
        {
            j++;
            curr_sum+=v[j];
            if(best_diff == -1 || abs(curr_sum - k) < best_diff)
            {
                best_diff = abs(curr_sum - k);
                best_i = i;
                best_j = j;
            }
        }
        if(j<n && (best_diff == -1 || abs(curr_sum - k) < best_diff))
        {
            best_diff = abs(curr_sum - k);
            best_i = i;
            best_j = j;
        }
        curr_sum-=v[i];
    }
    cout<<best_i<<" "<<best_j<<endl;
}


int main() {
	ios_base::sync_with_stdio(false);
    int t;
    cin>>t;
    for(int i_=0;i_<t;i_++) solve();

	return 0;
}
