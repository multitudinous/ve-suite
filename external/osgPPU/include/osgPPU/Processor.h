/***************************************************************************
 *   Copyright (c) 2008   Art Tevs                                         *
 *                                                                         *
 *   This library is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 3 of        *
 *   the License, or (at your option) any later version.                   *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU Lesse General Public License for more details.                    *
 *                                                                         *
 *   The full license is in LICENSE file included with this distribution.  *
 ***************************************************************************/

#ifndef _C_PROCESSOR__H_
#define _C_PROCESSOR__H_


//-------------------------------------------------------------------------
// Includes
//-------------------------------------------------------------------------
#include <osgPPU/Unit.h>
#include <osg/Camera>
#include <osg/State>
#include <osg/Geode>

#include <osgPPU/Export.h>


namespace osgPPU
{

class Visitor;

//! Main processor used to setup the unit pipeline
/**
 * The processor acts as a group node. The underlying graph can contain
 * units or other kind of nodes. However only units can be drawed in the apropriate
 * way.
 * The attached camera must provide a valid viewport and color attachment (texture)
 * which will be used as input for the pipeline.
 * 
 * The ppus are applied in a pipeline, so the output of one 
 * ppu is an input to the next one. At the end of the pipeline there should be
 * a bypassout ppu specified which do render the result into the frame buffer.
 * 
 * A processor can also be used to do some multipass computation on input data.
 * In that case it is not neccessary to output the resulting data on the screen, but
 * you can use the output texture of the last ppu for any other purpose.
 **/
class OSGPPU_EXPORT Processor : public osg::Group {
    public: 
    
        META_Node(osgPPU, Processor);

        /**
         * Initialize the ppu system.
        **/
        Processor();
         
        Processor(const Processor&, const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY);
    
        /**
         * Release the system. This will free used memory and close all ppus.
        **/
        virtual ~Processor();
        
        /**
        * Traverse method to traverse the subgraph. The unit pipeline will be updated
        * and drawed on the according node visitor types. The visitor has to provide
        * valid osg::FrameStamp so that the time get updated too.
        **/
        virtual void traverse(osg::NodeVisitor& nv);
        
        /**
         * Add a camera which texture attachment can be used as input to the pipeline.
         * The camera object must be setted up to render into a texture.
         * A bypass ppu (Unit) as first in the pipeline can bypass
         * the camera attachment into the pipeline.
         * @param camera Camera object to use input from.
         **/
        void setCamera(osg::Camera* camera);
        
        /**
         * Get camera used for this pipeline. This method returns the camera object
         * specified with setCamera().
        **/
        inline const osg::Camera* getCamera() const { return mCamera.get(); }
        inline osg::Camera* getCamera() { return mCamera.get(); }

        /**
        * Mark the underlying unit subgraph as dirty. This is required as soon
        * as you have changed the unit graph. Call this method to let processor
        * initilize  the underlying graph properly (setup all inputs and so on).
        **/
        inline void dirtyUnitSubgraph() {mbDirtyUnitGraph = true;}
               
        /**
        * Check whenever the subgraph is valid. A subgraph is valid if it can be
        * traversed by default osg traversal's, hence if it does not contain any cycles.
        * You have to traverse the processor with a
        * CullTraverser first to resolve the cycles automatically. Afterwards the subgraph 
        * became valid.
        **/
        inline bool isDirtyUnitSubgraph() const {return mbDirtyUnitGraph;}

        /**
        * Force to mark the subgraph as non-dirty. It is not recommended to traverse 
        * the graph without initializing it first. Otherwise there could be
        * cycles which will end up in seg faults. Use this method only if 
        * you know what you are doing.
        **/
        inline void markUnitSubgraphNonDirty() {mbDirtyUnitGraph = false;}

        /**
        * Search in the subgraph for a unit. To be able to find the unit 
        * you have to use unique names for it, however this is not a strict rule.
        * If nothing found return NULL.
        * @param name Unique name of the unit.
        **/
        Unit* findUnit(const std::string& name);

        /**
        * Remove a unit from the processor's subgraph. The method will 
        * use the visitor to remove the unit from the graph. The subgraph of the unit 
        * will be marked as dirty, so that it gets reorganized on the next traverse. All the
        * input units of the removed unit will be afterwards input units for the children
        * of the removed unit.
        * @param unit Pointer to the unit to remove
        * @return true on success otherwise false
        **/
        bool removeUnit(Unit* unit);

        /**
        * Overridden method from osg::Node to allow computation of bounding box.
        * This is needed to prevent traversion of this computation down to all childs.
        * This method do always returns empty bounding sphere.
        **/
        inline osg::BoundingSphere computeBound() const
        {
            return osg::BoundingSphere();
        }

    protected:

        /**
        * Init method which will be called automagically if processor became dirty.
        **/
        virtual void init();

        osg::observer_ptr<osg::Camera> mCamera;
        osg::ref_ptr<Visitor>   mVisitor;

        friend class Visitor;

        /**
        * Callback method which will be called as soon as a unit is get initialized.
        * Use this method to catch up the initialization process of a unit. 
        * @param unit Pointer to the unit which is initialized
        **/
        virtual void onUnitInit(Unit* unit) {}

        /**
        * Callback method for derived classes to detect whenever a unit is get updated.
        * This method is called once per frame for every unit whenever it is updated.
        * @param unit Pointer to the unit which is updated
        **/
        virtual void onUnitUpdate(Unit* unit) {}

    private:


        bool      mbDirty;
        bool      mbDirtyUnitGraph;
};


};

#endif
