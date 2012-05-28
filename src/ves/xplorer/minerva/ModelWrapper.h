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

///////////////////////////////////////////////////////////////////////////////
//
//  Wrapper around Model.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_MODEL_WRAPPER_H
#define VES_XPLORER_MINERVA_MODEL_WRAPPER_H

#include <ves/VEConfig.h>

#include <Minerva/Version.h>
#include <Minerva/Core/Data/Model.h>
#include <Minerva/Common/IElevationChangedListener.h>
#include <Minerva/Common/IElevationDatabase.h>
#include <Minerva/Common/IPlanetCoordinates.h>

#include <osg/Vec3d>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class CADEntity;
}
}
}
namespace Minerva
{
namespace Core
{
namespace Data
{
class DataObject;
}
}
}

namespace ves
{
namespace xplorer
{
namespace minerva
{


class VE_XPLORER_EXPORTS ModelWrapper :
    public Minerva::Core::Data::Model
{
public:

    typedef Minerva::Core::Data::Model BaseClass;
    typedef ves::xplorer::scenegraph::CADEntity CADEntity;
    typedef BaseClass::Extents Extents;

    USUL_DECLARE_REF_POINTERS( ModelWrapper );

    ModelWrapper();

    /// Elevation has changed within given extents (IElevationChangeListener).
    virtual bool elevationChangedNotify(
        const Extents& extents,
        unsigned int level,
        ElevationDataPtr elevationData,
        Usul::Interfaces::IUnknown* caller = 0x0 );

    /// Set/get the cad entity.
    void SetCADEntity( CADEntity* );
    CADEntity* GetCADEntity() const;

    void UpdateMatrix( Minerva::Common::IPlanetCoordinates* planet, Minerva::Common::IElevationDatabase* elevation );

    // Set the translation offset of cad in feet.
    void setTranslationOffset( double x, double y, double z );

    // Set/get the parent.
    void SetParent( Minerva::Core::Data::DataObject* parent );
    Minerva::Core::Data::DataObject* GetParent() const;

protected:

    virtual ~ModelWrapper();

private:

    CADEntity* _cadEntity;

    osg::Vec3d _offset;
    Minerva::Core::Data::DataObject* _parent;
};


}
}
}


#endif // VES_XPLORER_MINERVA_MODEL_WRAPPER_H
