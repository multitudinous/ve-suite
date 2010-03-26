#pragma once

#include <ves/xplorer/data/PropertySet.h>

#include <ves/open/xml/DataValuePairPtr.h>

#include <ves/open/xml/CommandPtr.h>

#include <string>

namespace ves
{
namespace conductor{


class ContourFeatureMaker
{
public:
    ContourFeatureMaker( );
    ContourFeatureMaker( const ContourFeatureMaker& orig );
    virtual ~ContourFeatureMaker( );
    void update( const std::string& dbFile, unsigned int recordID );

protected:
    void _addPlane( xplorer::data::PropertySet& set );
    void _updateContourInformation( xplorer::data::PropertySet& set );
    void _updateAdvancedSettings( xplorer::data::PropertySet& set );
    void _updateBaseInformation( xplorer::data::PropertySet& set );
    void SendUpdatedSettingsToXplorer( ves::open::xml::CommandPtr subDialogCommand, xplorer::data::PropertySet& set );
    

private:
    std::vector<ves::open::xml::DataValuePairPtr> _advancedSettings;///<The advanced settings.
    std::vector<ves::open::xml::DataValuePairPtr> _contourInformation;///<The countour setting data

    std::vector<ves::open::xml::DataValuePairPtr> _vistabBaseInformation;///<The basic information from the vistab

    std::string _commandName;///<The name of the command to send back

};

} // namespace conductor
} // namespace ves
