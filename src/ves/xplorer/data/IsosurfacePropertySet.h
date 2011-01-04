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
#ifndef VES_XPLORER_DATA_ISOSURFACEPROPERTYSET_H
#define	VES_XPLORER_DATA_ISOSURFACEPROPERTYSET_H

#include <ves/xplorer/data/PropertySet.h>
#include <ves/xplorer/data/PropertyPtr.h>

#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace data
{
/*!\file IsosurfacePropertySet.h
 *
 */

/*!\class ves::xplorer::data::IsosurfacePropertySet
 *
 */

/*!\namespace ves::xplorer::data
 *
 */
class VE_DATA_EXPORTS IsosurfacePropertySet : public PropertySet
{
public:
    ///Constructor
    IsosurfacePropertySet();
    ///Copy Contructor
    IsosurfacePropertySet( const IsosurfacePropertySet& orig );
    ///Destructor
    virtual ~IsosurfacePropertySet();

private:
    ///Update method
    void UpdateModeOptions( PropertyPtr property );
    ///Validate method
    bool ValidateScalarMinMax( PropertyPtr property, boost::any value );
    ///Validate method
    bool ValidateColorByScalarMinMax( PropertyPtr property, boost::any value );
    ///Update method
    void UpdateScalarDataOptions( PropertyPtr property );
    ///Update method
    void UpdateScalarDataRange( PropertyPtr property );
    ///Update the color by scalar
    void UpdateColorByScalarDataRange( PropertyPtr property );
    ///Update method
    void UpdateVectorDataOptions( PropertyPtr property );

private:
    ///Create the skeleton
    void CreateSkeleton();
};

} // namespace data
} // namespace xplorer
} // namespace ves

#endif	/* VES_XPLORER_DATA_ISOSURFACEPROPERTYSET_H */

