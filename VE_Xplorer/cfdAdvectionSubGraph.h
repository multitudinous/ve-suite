#ifndef CFD_AVDECTION_SUBGRAPH_H
#define CFD_AVDECTION_SUBGRAPH_H
#ifdef _OSG
#ifdef CFD_USE_SHADERS
class cfdTextureManager;

#include <osg/TexGenNode>

osg::ref_ptr<osg::TexGenNode> CreateAdvectionSubGraph(cfdTextureManager* tm);//unsigned int w,
		                                           //unsigned int h);
#endif// CFD_USE_SHADERS
#endif// _OSG
#endif //CFD_AVDECTION_SUBGRAPH_H
