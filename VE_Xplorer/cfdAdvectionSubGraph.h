#ifndef CFD_AVDECTION_SUBGRAPH_H
#define CFD_AVDECTION_SUBGRAPH_H
#ifdef _OSG
#ifdef CFD_USE_SHADERS
namespace osg{
   class Node;
}	
osg::Node* CreateAdvectionSubGraph(unsigned int w,
		                    unsigned int h);
#endif// CFD_USE_SHADERS
#endif// _OSG
#endif //CFD_AVDECTION_SUBGRAPH_H
