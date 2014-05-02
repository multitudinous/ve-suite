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
#ifndef VES_XPLORER_DATA_POLYDATAPROPERTYSET_H
#define	VES_XPLORER_DATA_POLYDATAPROPERTYSET_H

#include <propertystore/PropertySet.h>
#include <propertystore/PropertyPtr.h>
#include <ves/xplorer/data/VizBasePropertySet.h>

#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace data
{
/*!\file PolydataPropertySet.h
 * \class ves::xplorer::data::PolydataPropertySet
 * \namespace ves::xplorer::data
 *
 */
class VE_DATA_EXPORTS PolydataPropertySet : public VizBasePropertySet
{
public:
    ///Constructor
    PolydataPropertySet();
    ///Copy Contructor
    PolydataPropertySet( const PolydataPropertySet& orig );
    ///Destructor
    virtual ~PolydataPropertySet();

    /// Factory ctor
    virtual propertystore::PropertySetPtr CreateNew();

    virtual void EnableLiveProperties( bool live );

protected:
    ///Validate method
    bool ValidateColorByScalarMinMax( propertystore::PropertyPtr property, boost::any value );
    ///Update the color by scalar
    void UpdateColorByScalarDataRange( propertystore::PropertyPtr property );
	///Update warping
    void UpdateWarping( propertystore::PropertyPtr property );
	//Update particle data
	void UpdateParticleData( propertystore::PropertyPtr property );
	//Update transient data options for particle data
	void UpdateTransientDataOptions( propertystore::PropertyPtr property );
    ///Create the skeleton
	virtual void CreateSkeletonLfxDs();
    virtual void CreateSkeleton();

private:
	typedef switchwire::Event< void ( bool, double, bool ) > Update_polyData;
	Update_polyData m_updateLfxVtkPolyData;
};

} // namespace data
} // namespace xplorer
} // namespace ves

#endif	/* VES_XPLORER_DATA_POLYDATAPROPERTYSET_H */

