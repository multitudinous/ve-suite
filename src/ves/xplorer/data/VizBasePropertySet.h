/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef VES_XPLORER_DATA_VIZBASEPROPERTYSET_H
#define VES_XPLORER_DATA_VIZBASEPROPERTYSET_H

#include <ves/xplorer/data/PropertySet.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace data
{
/*!\file VizBasePropertySet.h
 * \class ves::xplorer::data::VizBasePropertySet
 * \namespace ves::xplorer::data
 *
 */
class VE_DATA_EXPORTS VizBasePropertySet : public PropertySet
{
public:
    ///Constructor
    VizBasePropertySet();
    ///Copy Contructor
    VizBasePropertySet( const VizBasePropertySet& orig );
    ///Destructor
    virtual ~VizBasePropertySet();

    ///Update method
    void UpdateModeOptions( PropertyPtr property );
    ///Validate method
    bool ValidateScalarMinMax( PropertyPtr property, boost::any value );
    ///Update method
    void UpdateScalarDataOptions( PropertyPtr property );
    ///Update method
    void UpdateScalarDataRange( PropertyPtr property );
    ///Update method
    void UpdateVectorDataOptions( PropertyPtr property );

protected:
    ///Registration of this property set for a child case
    ///\param tableName The table to be registered
    ///\post The PropertySet can now populate this table or read this table
    void RegisterPropertySet( std::string const& tableName );
    ///Create the skeleton for populating this PropertSet
    virtual void CreateSkeleton(){;}
    ///Delete this PropertySet from the DB. We override this from PropertySet.
    ///\param session The DB sesion holding this data
    ///\param TableName The TableName we are deleting
    virtual bool DeleteFromDatabase( Poco::Data::Session* const session, std::string const& TableName );

    ///Write this PropertySet to the DB. We override this from PropertySet
    ///\param session The DB sesion holding this data
    ///\param TableName The TableName we are deleting
    ///\param statement The POCO write statement with params
    virtual bool WriteToDatabase( Poco::Data::Session* const session, std::string const& TableName, Poco::Data::Statement& statement );

    ///Signal to generate deleting a viz feature
    //typedef boost::signals2::signal< void ( std::string const& ) > DeleteVizFeatureSignal_type;
    ///The delete viz signal
    ves::util::StringSignal_type m_deleteVizSignal;
    ///The add viz signal
    ves::util::TwoStringSignal_type m_addVizSignal;
    
private:
};
} // namespace data
} // namespace xplorer
} // namespace ves

#endif	/* VES_XPLORER_DATA_VIZBASEPROPERTYSET_H */
