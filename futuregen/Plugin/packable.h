#ifndef PACKABLE_H
#define PACKABLE_H

#include <string>
#include "string_ops.h"

#ifdef _WIN32
#pragma warning(disable : 4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#endif

class packable {

public:

  packable  () {};
  virtual ~packable () {};

  virtual bool pack   (std::string &packed) =0;
  virtual bool unpack (std::string packed)  =0;

  void pack_ids(std::string &packed)
    {
      int s;
      
      std::string type = to_string(_type);
      s = type.size();
      type.append(24-s, ' ');
      
      std::string category = to_string(_category);
      s = category.size();
      category.append(24-s, ' ');
      
      std::string id = to_string(_id);
      s = id.size();
      id.append(24-s, ' ');
      
      packed = type + category + id;
    }

  void unpack_ids(const char *packed)
    {
      char buf[25];
     
      strncpy(buf, &packed[0], 24);
      buf[24]='\0';
      _type = atoi(buf);
      
      strncpy(buf, &packed[24], 24);
      buf[24]='\0';
      _category = atoi(buf);
      
      strncpy(buf, &packed[48], 24);
      buf[24]='\0';
      _id = atoi(buf);
    }

  int _type;
  int _category;
  int _id;
};

#endif
