#pragma once

#include <ves/xplorer/data/PropertySet.h>
#include <ves/xplorer/data/PropertyPtr.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <ves/VEConfig.h>

#include <boost/signals2/signal.hpp>

namespace ves
{
namespace xplorer
{
namespace data
{
/*!\file DatabaseDetailsPropertySet.h
 * \class ves::xplorer::data::DatabaseDetailsPropertySet
 * \namespace ves::xplorer::data
 *
 */
class VE_DATA_EXPORTS DatabaseDetailsPropertySet : public PropertySet
{
public:
    ///Constructor
    DatabaseDetailsPropertySet();
    ///Copy Contructor
    DatabaseDetailsPropertySet( const DatabaseDetailsPropertySet& orig );
    ///Destructor
    virtual ~DatabaseDetailsPropertySet();

private:
    ///Create the skeleton
    void CreateSkeleton();

};
} // namespace data
} // namespace xplorer
} // namespace ves
