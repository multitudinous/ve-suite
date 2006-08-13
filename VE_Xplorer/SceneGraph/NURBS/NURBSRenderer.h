#ifndef NURBS_SURFACE_RENDER_H
#define NURBS_SURFACE_RENDER_H

#include <osg/Geode>
#include "VE_Xplorer/SceneGraph/NURBS/Export.h"
#include "VE_Xplorer/SceneGraph/NURBS/NURBSObject.h"

/*!\file NURBSRenderer.h
  NURBS Object OSG Renderer API
  */
/*!\class NURBS::NURBSRenderer
 * Class defining the interface between NURBS object and osg::Geometry.
 */

namespace NURBS
{


class VE_NURBS_EXPORTS NURBSRenderer
{
public:
   ///Constructor
   NURBSRenderer(NURBS::NURBSObject* object);

   ///Destructor
   virtual ~NURBSRenderer();

   ///Set the NURBS::NURBSObject
   ///\param object The NURBS::NURBSObject
   void SetNURBSObject(NURBS::NURBSObject* object);

   ///Show the triangulated wireframe surface
   ///\param trueFalse turn off/on wireframe
   void ViewWireframe(bool trueFalse);

   ///Get the original surface
   NURBS::NURBSObject* GetNURBS();

   ///Get the osg::Geometry for the surface
   osg::Geode* GetTriangulatedSurface();

   ///Get the osg::Geometry for the control mesh
   osg::Geode* GetControlMesh();
protected:
   bool _retessellate;///<Update the mesh

   bool _wireframeView;///<View the wireframe (tessellation)
   
   ///Tessellate the surface
   void _tessellateSurface();

   ///Tessellate the curve
   void _tessellateCurve();
   
   ///Draw the control mesh
   void _updateControlMesh();

   NURBS::NURBSObject* _nurbsObject;///<The NURBSurface
   osg::ref_ptr<osg::Geode> _triangulatedSurface;///<The triangulated surface
   osg::ref_ptr<osg::Geode> _controlMesh;///<The control mesh

}
;
}
#endif //NURBS_SURFACE_RENDER_H