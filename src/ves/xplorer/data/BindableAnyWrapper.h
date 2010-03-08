#ifndef _BINDABLEANYWRAPPER_H
#define	_BINDABLEANYWRAPPER_H

#include <string>

// TODO: cull include deps
#include <boost/any.hpp>
#include <Poco/Data/Common.h>
#include <Poco/Data/SQLite/Connector.h>

/// @file BindableAnyWrapper.h

/// @class BindableAnyWrapper
/// This class provides a means to bind values returned on the stack to a
/// Poco::Data::Statement. Poco::Data::Statement expects a constant reference
/// for the binding. When dealing with the return value of functions

namespace ves
{
namespace xplorer
{
namespace data
{

class BindableAnyWrapper
{
public:
    BindableAnyWrapper( );
    BindableAnyWrapper( const BindableAnyWrapper& orig );
    virtual ~BindableAnyWrapper( );

    bool BindValue( Poco::Data::Statement *statement, boost::any value );
private:
    bool mBool;
    int mInt;
    float mFloat;
    double mDouble;
    std::string mString;

};

} // namespace data
} // namespace xplorer
} // namespace ves

#endif	/* _BINDABLEANYWRAPPER_H */

