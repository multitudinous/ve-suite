#include "summary_values.h"

///////////////////////////////////////////////////////////////////////////////////////

summary_values::summary_values ()
{ 
}

///////////////////////////////////////////////////////////////////////////////////////

summary_values::~summary_values ()
{
}

///////////////////////////////////////////////////////////////////////////////////////

void summary_values::clear ()
{
  summaries.clear();
}

///////////////////////////////////////////////////////////////////////////////////////

void summary_values::insert_summary_val (char *description, int value)
{
  insert_summary_val(description, (double)value);
}

///////////////////////////////////////////////////////////////////////////////////////

void summary_values::insert_summary_val (char *description, float value)
{
  insert_summary_val(description, (double)value);
}

///////////////////////////////////////////////////////////////////////////////////////

void summary_values::insert_summary_val (char *description, double value)
{
  //printf("insertval double called\n");
  int i, j, description_end;
  std::string desc, format, units;
  char f[50], u[50], val[100];
 
  desc = std::string(description);
  int loc_1 = desc.find("FORMAT:");
  int loc_2 = desc.find("UNITS:");

  // Extract description string.
  if((loc_1 != -1) && (loc_2 != -1))

#ifndef WIN32
    description_end = min(loc_1, loc_2);
#else
    description_end = __min(loc_1, loc_2);
#endif

  else if(loc_1!=-1) 
    description_end = loc_1;
  else if(loc_2!=-1)
    description_end = loc_2;
  else 
    description_end = desc.length();
  desc.assign(desc, 0, description_end);
  
  // Extract format string.
  if(sscanf(description+loc_1, "%s", f) != 1)
    format = "5.1E";
  else {
    format = f;
    format.replace(0, 7, "");
  }
  format = "%" + format;
  strcpy(f, format.c_str());

  // Extract units string.
  if(sscanf(description+loc_2, "%s", u) != 1)
    units = "NO UNITS";
  else {
    units = u;
    units.replace(0, 6, "");
  }

  // do units
  //UnitConverter uc;
  //unit_type default_units = si;
  //UnitPair up = uc.convert(u, (float)(value), default_units);
  //desc += " " + up.first;
  //sprintf(val, f, up.second);
  //sprintf(val, "%g", value);
  sprintf(val, f, value);
  desc = desc + "(" + units + ")";
     
  std::pair<string, string> tmp_pair(desc, val);
  
  // If description is in current summary values list then replace pair.
  // Else insert at end of summary value list.
  j = summaries.size();
  for(i=0; i<j; i++)
    if(summaries[i].first == tmp_pair.first)
      j = i;
  if(j == (int)summaries.size())
    summaries.push_back(tmp_pair);
  else
    summaries[j] = tmp_pair;
}

///////////////////////////////////////////////////////////////////////////////////////

// Insert "as is".
void summary_values::insert_summary_val(char *description, char *value) {
  int i,j;
  std::pair<std::string, std::string> tmp_pair(description, value);

  // If description is in current summary values list then replace pair.
  // Else insert at end of summary value list.
  j = summaries.size();
  for(i=0; i<j; i++)
    if(summaries[i].first == tmp_pair.first)
      j = i;
  if(j == (int)summaries.size())
    summaries.push_back(tmp_pair);
  else
    summaries[j] = tmp_pair; 
}

///////////////////////////////////////////////////////////////////////////////////////

int summary_values::size () 
{
  return summaries.size();
}

///////////////////////////////////////////////////////////////////////////////////////

string summary_values::get_description (int index)
{
  return summaries[index].first;
}

///////////////////////////////////////////////////////////////////////////////////////

string summary_values::get_value (int index)
{
  return summaries[index].second;
}
