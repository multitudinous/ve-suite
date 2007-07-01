//
// Copyright (C) 2007 Skew Matrix Software LLC (http://www.skew-matrix.com)
//
// This library is open source and may be redistributed and/or modified under  
// the terms of the OpenSceneGraph Public License (OSGPL) version 0.0 or 
// (at your option) any later version.  The full license is in LICENSE file
// included with this distribution, and on the openscenegraph.org website.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// OpenSceneGraph Public License for more details.
//

#ifndef OSGOQ_OCCLUSION_QUERY_CONTEXT_H
#define OSGOQ_OCCLUSION_QUERY_CONTEXT_H 1

#include "osgOQ/ExportDeclaration.h"
#include <osg/Referenced>
#include <osg/Drawable>

namespace osgOQ {

class OSGOQ_EXPORT OcclusionQueryContext : public osg::Referenced
{
public:
    OcclusionQueryContext();
    ~OcclusionQueryContext();

    // Sets/gets the visibility threshold. If the test indicates that
    //   the number of visible pixels is less than the specified
    //   threshold, don't draw the actual geometry.
    void setVisibilityThreshold( unsigned int pixels ) { _visThreshold = pixels; }
    unsigned int getVisibilityThreshold() const { return _visThreshold; }

    // Specify the vertex count threshold for performing occlusion
    //   query tests. Nodes in the scene graph whose total child geometry
    //   contains fewer vertices than the specified threshold will
    //   never be tested, just drawn. (In fact, they will br treated as
    //   potential occluders and rendered first in front-to-back order.)
    void setOccluderThreshold( int vertices ) { _occluderThreshold = vertices; }
    int getOccluderThreshold() const { return _occluderThreshold; }

	// Controls whether or not OQNs can be nested. If true, a given NodePath
	//   could contain multiple OQNs. If false, a given NodePath can only
	//   contain one OQN.
    void setNonFlatPlacement( bool useNonFlat ) { _useNonFlat = useNonFlat; }
    bool getNonFlatPlacement() const { return _useNonFlat; }

    // Specifies how many frames to wait before issuing another query.
    void setQueryFrameCount( int frames ) { _queryFrameCount = frames; }
    int getQueryFrameCount() const { return _queryFrameCount; }

	// Indicate whether or not the bounding box used in the occlusion query test
	//   should be rendered. Handy for debugging and development.
    void setDebugDisplay( bool enable ) { _debugBB = enable; }
    bool getDebugDisplay() const { return _debugBB; }

	// Sets the debug verbosity. Currently supported 'level' values:
	//    0 -- Verbosity is controlled by osg::notify.
	//    1 -- For each OQN in each frame, displays whether that node
	//         thinks its actual geometry is visible or not and why.
	// Call through OcclusionQueryRoot to set value only for a
	//   specific number of frames.
	void setDebugVerbosity( int level ) { _debugVerbosity = level; }
	int getDebugVerbosity() const { return _debugVerbosity; }


    // When an OQR creates all OQNs and each OQN shares the same OQC,
    //   these methods are used to uniquely name all OQNs. Handy
    //   for debugging.
	std::string getNextOQNName();
	int getNameIdx() const { return _nameIdx; }

    // Another advantage to using one OQC for all OQNs is that the OQNs
    //   can then share these StateSets.
    // Get a StateSet to use when rendering test/debug geometry
    osg::StateSet* getTestStateSet() { return _state.get(); }
    osg::StateSet* getDebugStateSet() { return _debugState.get(); }


    // Enables/disables statistics tracking (yet another reason to share
    //   OQCs between OQNs; common statistics gathering).
	void setStatistics( bool stats=true ) { _statistics = stats; }
	bool getStatistics() const { return _statistics; }

	void incNumQueries() { _numQueries++; }
	void incNumPassed() { _numPassed++; }
	void incNumFailed() { _numFailed++; }
	unsigned int getNumQueries() { return _numQueries; }
	unsigned int getNumPassed() { return _numPassed; }
	unsigned int getNumFailed() { return _numFailed; }

	void clearStatistics() { _numQueries = _numPassed = _numFailed = 0; }

protected:
    // User-settable variables
    unsigned int _visThreshold;
    int _occluderThreshold;
	bool _debugBB;
	bool _useNonFlat;
    int _queryFrameCount;


    osg::ref_ptr<osg::StateSet> _state;
    osg::ref_ptr<osg::StateSet> _debugState;

	unsigned int _nameIdx;

	bool _statistics;
	unsigned int _numQueries;
	unsigned int _numPassed;
	unsigned int _numFailed;
	int _debugVerbosity;

	static std::string _visibilityThresholdToken;
	static std::string _occluderThresholdToken;
	static std::string _debugBoundingVolumeToken;
	static std::string _placementToken;
    static std::string _queryFrameCountToken;
};

}


#endif
