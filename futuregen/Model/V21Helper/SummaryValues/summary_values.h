#ifndef SUMMARY_VALUES_H
#define SUMMARY_VALUES_H

#include <stdio.h>
#include <string_ops.h>
#include "unit_conversion.h"

#include <vector>
#include <string>

class summary_values {

public:

  summary_values  ();
  ~summary_values ();

  void clear();

  void insert_summary_val (char *description, int value);
  void insert_summary_val (char *description, float value);
  void insert_summary_val (char *description, double value);
  void insert_summary_val (char *description, char *value);

  int size ();

  std::string get_description (int index);
  std::string get_value       (int index);
 
private:

  std::vector<std::pair<std::string, std::string> > summaries;

};

#endif
