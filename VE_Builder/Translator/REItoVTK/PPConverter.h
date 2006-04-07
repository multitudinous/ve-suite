#ifndef PPCONVERTER_H
#define PPCONVERTER_H

#include <string>

using namespace std;

class PPConverter {

public:
  PPConverter  (std::string pp1_file, std::string pp3_file);
  ~PPConverter ();
  
  int makeVTK (std::string pp_file, std::string pd_file);

private:
  void swap_4_range (char *mem_ptr1, int num);
  void swap_4       (char* data);

  std::string pplot1_file;
  std::string pplot3_file;
};

#endif
