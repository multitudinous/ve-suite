#ifndef FC_PROCESS_H
#define FC_PROCESS_H

#include <string>
#include <vector>
#include <map>

namespace Vision21 {

using namespace::std;

class FC_Process
{
public:

  FC_Process(char f) { flag=f; };
  map<string, double> anode_s;
  map<string, double> cathode_s;
  double anode_T;
  double anode_P;
  double anode_U; //Utilization
  double cathode_T;
  double cathode_P;
  double cathode_U; //Utilization

  double Produced_Power;
  double Overall_Q;

  bool parse(string input_path);
  void dump();

private:

  static const int BUFFER_MAX = 4098;

  int get_token(char* current_line, vector<string>& toks);
  int get_token2(char* current_line, vector<string>& toks);
  bool match(const string& s1, const string& s2);
  void process();

  vector<string> names;
  vector<double> values;
  char flag;
};

} // end namespace Vision21

#endif
