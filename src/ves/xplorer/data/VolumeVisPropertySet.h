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
#ifndef VES_XPLORER_DATA_VOLUMEVISPROPERTYSET_H
#define VES_XPLORER_DATA_VOLUMEVISPROPERTYSET_H

#include <propertystore/PropertySet.h>
#include <propertystore/PropertyPtr.h>
#include <ves/xplorer/data/VizBasePropertySet.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <ves/VEConfig.h>

#include <switchwire/Event.h>

namespace ves
{
namespace xplorer
{
namespace data
{

	/*
class VE_DATA_EXPORTS IVisProps
{
public:
	class Prop
	{
	public:
		Prop();
		Prop(const std::string &name, const boost::any &value, const std::string &dispName)
		{
			_name = name;
			_value = value;
			_dispName = dispName;
		}

		std::string _name;
		boost::any	_value;
		std::string _dispName;
	};

	typedef boost::shared_ptr<Prop> PropPtr;

public:
	IVisProps(){}
	virtual void UpdateProperty( const std::string &name, const boost::any &value ) = 0;
	virtual void GetProperties(std::vector< PropPtr > *pProps) = 0;
};

typedef boost::shared_ptr<IVisProps> IVisPropsPtr;
*/

/*!\file VolumeVisPropertySet.h
 * \class ves::xplorer::data::VolumeVisPropertySet
 * \namespace ves::xplorer::data
 *
 */
class VE_DATA_EXPORTS VolumeVisPropertySet : public VizBasePropertySet
{
public:
    ///Constructor
    VolumeVisPropertySet();
    ///Copy Contructor
    VolumeVisPropertySet( const VolumeVisPropertySet& orig );
    ///Destructor
    virtual ~VolumeVisPropertySet();

    /// Factory ctor
    virtual propertystore::PropertySetPtr CreateNew();

    ///Setup the live properties
    virtual void EnableLiveProperties( bool live );

protected:
    ///Slot connected to the value change of  display seed points
    ///\param property The bool value for the seed point display flag
    //void UpdateSeedPointDisplay( propertystore::PropertyPtr property );

	virtual void CreateSkeletonLfxDs();

    ///Create the skeleton
    virtual void CreateSkeleton();

    void ActivateVoilumeVis();

    void UpdateScalarRange( propertystore::PropertyPtr property );

    void UpdateScalar( propertystore::PropertyPtr property );

	void UpdateLfxDataset( propertystore::PropertyPtr property );
	void UpdateLfxChannel( propertystore::PropertyPtr property );
	void UpdateLfxUniform( propertystore::PropertyPtr property );
	

    ///????
    ves::util::TwoDoubleSignal_type m_updateTBETScalarRange;
    ///????
    typedef switchwire::Event< void ( std::string const&, std::string const&, double const&, double const& ) > UpdateScalar_type;
    UpdateScalar_type m_updateTBETScalar;


    /*
        ///Update signal to control turning off and on seed points
        typedef switchwire::Event< void ( const std::string&, const bool ) > ActivateSeedPointsSignal_type;
        ActivateSeedPointsSignal_type m_activateSeedPoints;
        ///Update signal to control the bounding box for seed points
        typedef switchwire::Event< void ( const std::vector< double >& ) > UpdateSeedPointBoundsSignal_type;
        UpdateSeedPointBoundsSignal_type m_updateSeedPointBounds;
        ///Update signal to control changing the active dataset
        typedef switchwire::Event< void ( const std::string& ) > UpdateActiveDataSetSignal_type;
        UpdateActiveDataSetSignal_type m_activeDataSet;
    */

};

} // namespace data
} // namespace xplorer
} // namespace ves

#endif	/* VES_XPLORER_DATA_VOLUMEVISPROPERTYSET_H */
