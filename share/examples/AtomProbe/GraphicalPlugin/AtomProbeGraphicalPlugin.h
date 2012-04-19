/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef TANK_GRAPHICAL_PLUGIN_H
#define TANK_GRAPHICAL_PLUGIN_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>

#include <ves/open/xml/CommandPtr.h>

#include <osg/Switch>

#include "TankData.h"
#include <osg/Geometry>
#include <iostream>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SignalWrapper.h>
typedef boost::signals2::signal< void ( const std::vector< int >&, const std::vector< int >&, const std::vector< int >&, const std::vector< double >&  ) > ColorDoubleVectorSignal_type;

typedef boost::signals2::signal< void (  const std::vector< double >&  ) > DoubleVectorSignal_type;

namespace ves
{
namespace xplorer
{

namespace device
{
class KeyboardMouse;
}
} //end xplorer
} //end ves


namespace opcgp
{
class VE_USER_PLUGIN_EXPORTS AtomProbeGraphicalPlugin : public ves::xplorer::plugin::PluginBase
{
public:
    ///Constructor
    AtomProbeGraphicalPlugin();
    ///Destructor
    virtual ~AtomProbeGraphicalPlugin();

    ///Initialize the scenegraph with graphics models
    virtual void InitializeNode( osg::Group* veworldDCS );
    ///Check if anything needs updated graphically
    virtual void PreFrameUpdate();
    ///Process the current command in the queue from the dynsim unit
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );
    virtual void SetCurrentCommands( std::vector< ves::open::xml::CommandPtr > const& commands );
	void setColorizeData(int data);
	
	void setMaskVariable(int var) {
		currentMaskVariable = var;
		std::cout <<"set mask var gp" << std::endl;
		reinitializePipelineData();
		reMask(currentMaskVariable, currentMaskBottom, currentMaskTop); 
		hitMasking(hitEnabled);
		reColorize();
		colorizeByHits = false;
	
	}
	void setMaskBottom(double bottom) {
		currentMaskBottom = bottom;
		std::cout <<"set mask bottom gp" << std::endl;
		reinitializePipelineData();
		reMask(currentMaskVariable, currentMaskBottom, currentMaskTop); 
		hitMasking(hitEnabled);
		reColorize();
		colorizeByHits = false;
	}
	void setMaskTop(double top) {
		currentMaskTop = top;
		std::cout <<"set mask top gp" << std::endl;
		reinitializePipelineData();
		reMask(currentMaskVariable, currentMaskBottom, currentMaskTop); 
		hitMasking(hitEnabled);
		reColorize();
		colorizeByHits = false;
	}
	void setLastTime(double lt) {
		lastTime = lt;
		std::cout << "set last time" << std::endl;
		reinitializePipelineData();
		reMask(currentMaskVariable, currentMaskBottom, currentMaskTop); 
		hitMasking(hitEnabled);
		if(colorizeByHits)
			hitColorization(rrHit,ggHit,bbHit, hitEnabled);
		else 
			reColorize();
		
		
	}
	
	void setHitColorization(std::vector<int> rr, std::vector<int> gg, std::vector<int> bb, std::vector<bool> enabled) {
		reinitializePipelineData();
		reMask(currentMaskVariable, currentMaskBottom, currentMaskTop); 
		
		rrHit = rr;
		ggHit = gg;
		bbHit = bb;
		hitEnabled = enabled;
		hitMasking(hitEnabled);
		hitColorization(rrHit,ggHit,bbHit, hitEnabled);
		colorizeByHits = true;
	}
		
		
	/**
	 * Colorize the atoms using the selected variable and range
	 *
	 * This function colorizes the atoms based on the selected variable and the range of colorization
	 * This range is relative to the min and max of the selected variable. This function operates on the currently
	 * active set of atoms.
	 *
	 * @param variable the selected variable
	 * @param bottomClip the point in the selected variable's range that will correspond to the lowest
	 * value on the color ramp. Values below this will recieve the minimum value.
	 * @param topClip the point in the selected variable's range that will correspond to the highest value
	 * on the color ramp. Values above this will recieve the maximum value.
	 */
	void reColorize(int variable, float bottomClip, float topClip);
	void reColorize() {
		std::cout << currentVariable << " " << currentBottom << " " << currentTop << std::endl;
		reColorize(currentVariable, currentBottom, currentTop);
	}
	void reColorizeNm(std::vector<double> r, std::vector<double> g, std::vector<double> b);
	void reMaskNm(std::vector<bool> enabled);
	void hitMasking(std::vector<bool> enabled);
	
	
	void hitColorization(std::vector<int> rr, std::vector<int> gg, std::vector<int> bb, std::vector<bool> enabled);
	
	/**
	 * Mask the atoms using the selected variables and range
	 *
	 * This function masks or hides some of the atoms based on the selected variable. Atoms inside
	 * the range are shown. Atoms below and above the range are clipped. The range is based on a percentage
	 * of the minimum and maximum values of that variable and not on absolute values. 
	 *
	 * @param variable the selected variable
	 * @param bottomClip the percentage of the range below which atoms will not be shown
	 * @param topClip the percentage of the range above which atoms will not be shown
	 */
	void reMask(int variable, float bottomClip, float topClip);;
	
	/**
	 * This function uploads the current set of atoms to the card. This should be called after all
	 * processing is done.
	 */
	void uploadAtomstoCard() {}
	
	/**
	 * This function reinitializes the set of atoms in the processing pipeline prior to a pipeline reflow.
	 * 	
	 */
	void reinitializePipelineData();
	
private:
    void FindPartNodeAndHighlightNode();
    ///Keyboard helper pointer
    ves::xplorer::device::KeyboardMouse* m_keyboard;

    osg::ref_ptr< osg::Node > m_tankGeometry;
	osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_tankDCS;
    TankData* td;
    double m_tankLevel;
	
	osg::Vec3Array* vertices;
	osg::Switch* leswitch, *latticeSwitch;
	int framecount;
	
	bool latticeEnabled;
	bool dislocationsEnabled;
	bool animating;
	float* rawData;
	unsigned int _numAtoms;
	osg::Geometry* atomProbeGeometry;
	unsigned int currentVariable;
	float currentBottom;
	float currentTop;
	float currentMaskBottom, currentMaskTop;
	float currentMaskVariable;
	std::vector<double> histogram;
	std::vector<int> rr, gg, bb;
	std::vector<int> rrHit, ggHit, bbHit;
	std::vector<bool> hitEnabled;
	ColorDoubleVectorSignal_type m_HistogramDataSignal;
	DoubleVectorSignal_type m_MaskHistogramDataSignal;
	ves::util::DoubleSignal_type m_NumPulsesSignal;
	std::vector<unsigned int> currentAtoms;
	std::vector<double> timeIndex;
	double minTime, lastTime, maxTime;
	/// Required to be able to connect up to signals.
    ves::xplorer::eventmanager::ScopedConnectionList m_connections;
	
	bool colorizeByHits;
	//internal colour array (not the one sent to OSG
	
	osg::Vec4ubArray* colours, *coloursOnCard;
	

	
};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( AtomProbeGraphicalPlugin )
    
}
#endif //TANK_GRAPHICAL_PLUGIN_H
