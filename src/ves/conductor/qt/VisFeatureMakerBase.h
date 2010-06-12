#pragma once

#include <ves/xplorer/data/PropertySet.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>

#include <string>

namespace ves
{
namespace conductor
{

class VisFeatureMakerBase
{
public:
    VisFeatureMakerBase( );
    VisFeatureMakerBase( const VisFeatureMakerBase& orig );
    virtual ~VisFeatureMakerBase( );

protected:
    void _updateAdvancedSettings( ves::xplorer::data::PropertySet& set );
    void _updateBaseInformation( ves::xplorer::data::PropertySet& set );
    void SendUpdatedSettingsToXplorer( ves::open::xml::CommandPtr subDialogCommand, ves::xplorer::data::PropertySet& set );

    std::string _commandName;///<The name of the command to send back
    std::vector<ves::open::xml::DataValuePairPtr> _advancedSettings;///<The advanced settings.
    std::vector<ves::open::xml::DataValuePairPtr> _vistabBaseInformation;///<The basic information from the vistab
    
private:

};

} // namespace conductor
} // namespace ves
