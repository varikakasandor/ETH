#include <bits/stdc++.h>
using namespace std;
typedef long long ll;


ll rec(ll x, ll moves_left, vector<vector<pair<ll, ll> > >& g, vector<vector<ll> >& memo)
{
  if(moves_left == 0) return 0;
  else if(memo[x][moves_left] == -1)
  {
    if(g[x].empty())
    {
      if(x != 0) memo[x][moves_left] = rec(0, moves_left, g, memo);
    }
    else
    {
      for(pair<ll, ll> p: g[x]) memo[x][moves_left] = max(memo[x][moves_left], p.second + rec(p.first, moves_left - 1, g, memo));
    }
  }
  return memo[x][moves_left];
}

void solve()
{
  ll n, m, x, k;
  cin>>n>>m>>x>>k;
  vector<vector<pair<ll, ll> > > g(n);
  for(ll i=0;i<m;i++)
  {
    ll u, v, w;
    cin>>u>>v>>w;
    g[u].push_back(make_pair(v, w));
  }
  vector<vector<ll> > memo(n, vector<ll>(k+1, -1));
  for(ll moves_left = 1; moves_left <=k; moves_left++)
  {
    ll curr_score = rec(0, moves_left, g, memo);
    if(curr_score >= x)
    {
      cout<<moves_left<<endl;
      return;
    }
  }
  cout<<"Impossible"<<endl;
}

int main()
{
  ll tc;
  cin>>tc;
  for(ll i=0;i<tc;i++) solve();
  return 0;
}
