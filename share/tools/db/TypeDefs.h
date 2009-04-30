
#ifndef TYPE_DEFS_H
#define TYPE_DEFS_H

// --- POCO Includes --- //
#include <Poco/Tuple.h>

// --- C/C++ Includes --- //
#include <string>
#include <vector>

// --- typedefs --- //
//typedef Poco::Tuple< std::string > String1D;
typedef Poco::Tuple< std::vector< std::string > > String2D;
typedef std::vector< std::string > StringVector1D;
typedef std::vector< String2D > StringVector2D;

#endif //TYPE_DEFS_H
