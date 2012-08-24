/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#ifndef VES_XPLORER_DATA_PREFERENCESPROPERTYSET_H
#define VES_XPLORER_DATA_PREFERENCESPROPERTYSET_H

#include <propertystore/PropertySet.h>
#include <propertystore/PropertyPtr.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <ves/VEConfig.h>

#include <switchwire/Event.h>

namespace ves
{
namespace xplorer
{
namespace data
{
/*!\file PreferencesPropertySet.h
 * \class ves::xplorer::data::PreferencesPropertySet
 * \namespace ves::xplorer::data
 *
 */
class VE_DATA_EXPORTS PreferencesPropertySet : public propertystore::PropertySet
{
public:
    ///Constructor
    PreferencesPropertySet();
    ///Copy Contructor
    PreferencesPropertySet( const PreferencesPropertySet& orig );
    ///Destructor
    virtual ~PreferencesPropertySet();

private:
    ///Enable method
    void EnableNearFarRatio( propertystore::PropertyPtr& property );
    ///Enable method
    void EnableBackgroundColor( propertystore::PropertyPtr& property );
    ///Enable method
    void EnableDraggerScaling( propertystore::PropertyPtr& property );

    ///Relay method for passing signals on to xplorer
    void UpdateBackgroundColor( propertystore::PropertyPtr& property );
    ///Relay method for passing signals on to xplorer
    //void UpdateNavEqualZero( propertystore::PropertyPtr property );
    ///Relay method for passing signals on to xplorer
    //void UpdateNavGreaterZero( propertystore::PropertyPtr property );
    ///Relay method for passing signals on to xplorer
    //void UpdateShutdownXplorer( propertystore::PropertyPtr property );
    ///Relay method for passing signals on to xplorer
    //void UpdatePhysicsDebugger( propertystore::PropertyPtr property );
    ///Relay method for passing signals on to xplorer
    //void UpdateScriptLogger( propertystore::PropertyPtr property );
    ///Relay method for passing signals on to xplorer
    //void UpdateScreenAlignedNormals( propertystore::PropertyPtr property );
    ///Relay method for passing signals on to xplorer
    void UpdateDraggerScaling( propertystore::PropertyPtr& property );
    ///Relay method for passing signals on to xplorer
    void UpdateNearFarRatio( propertystore::PropertyPtr& property );
    ///Relay method for passing signals on to xplorer
    //void UpdateLODScaling( propertystore::PropertyPtr property );
    void SaveChanges( propertystore::PropertyPtr& property );

private:
    ///Create the skeleton
    void CreateSkeleton();

    ///Update signal containing new seed points dimensions
    typedef switchwire::Event< void ( bool const&, double const& ) > UpdateCheckAndValueSignal_type;
    UpdateCheckAndValueSignal_type m_nearFarRatio;
    UpdateCheckAndValueSignal_type m_draggerScaling;

    ///Update signal
    ves::util::BoolAndDoubleVectorSignal_type* m_backgroundColor;

    ///Update signal for check box preferences
    //    typedef switchwire::Event< void ( bool const& ) > CheckValueSignal_type;
    //    CheckValueSignal_type m_navZEqual0;
    //    CheckValueSignal_type m_navZGreater0;
    //    CheckValueSignal_type m_shutdownXplorer;
    //    CheckValueSignal_type m_physicsDebugger;
    //    CheckValueSignal_type m_scriptLogger;
    //    CheckValueSignal_type m_screenAlignedNormals;

    ///Update signal for check box preferences
    //    typedef switchwire::Event< void ( const double ) > DoubleValueSignal_type;
    //    DoubleValueSignal_type m_lodScaling;
};
} // namespace data
} // namespace xplorer
} // namespace ves

#endif	/* _PreferencesPropertySet_H */
