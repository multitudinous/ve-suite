#ifndef GETVESUITE_HeatX_HeatX_F-HT-2CO_H
#define GETVESUITE_HeatX_HeatX_F-HT-2CO_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_HeatX_HeatX_F-HT-2CO( void )
{
    unsigned char osgData[ 5515 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,53,0,142,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,250,43,246,161,255,0,130,205,126,200,159,183,159,195,223,218,155,199,159,12,116,143,141,159,179,203,124,72,255,0,130,76,126,218,223,178,245,167,132,126,52,252,60,180,211,252,79,251,85,252,82,248,239,165,124,63,215,63,100,40,180,219,143,128,94,37,241,190,143,117,225,239,3,127,103,254,209,150,176,94,252,64,213,188,52,190,23,185,253,170,46,155,195,81,221,90,248,147,198,247,90,103,220,209,127,193,215,255,0,177,20,218,173,238,135,31,236,141,255,0,5,11,125,87,78,211,180,189,90,246,204,120,7,246,85,15,6,159,172,220,234,246,122,101,195,72,127,108,13,140,178,220,232,58,178,128,172,89,77,153,46,20,50,22,252,78,253,144,191,101,207,248,72,63,100,223,217,127,94,254,196,150,97,173,126,206,255,0,4,245,127,52,90,74,222,104,212,190,26,248,102,247,204,12,33,33,131,121,249,7,60,231,52,120,107,246,92,51,126,214,95,26,244,31,236,73,79,246,111,236,239,251,47,234,222,80,180,144,148,254,219,248,151,251,94,217,249,155,4,95,40,99,160,17,158,135,201,35,36,138,240,214,99,21,95,17,203,31,123,173,239,246,90,142,159,126,166,138,9,165,169,253,191,254,195,159,183,23,236,239,255,0,5,15,253,157,60,23,251,78,126,204,254,45,151,196,126,3,241,92,75,103,171,232,122,205,189,190,147,227,239,134,94,52,183,211,180,221,75,93,248,103,241,67,195,16,222,220,31,11,248,239,78,182,213,244,201,101,133,103,185,177,191,211,245,109,63,93,208,239,245,111,14,234,218,70,175,127,86,203,246,219,248,87,117,241,47,195,127,8,46,60,61,241,11,76,248,133,174,254,215,126,56,253,139,245,29,6,247,74,240,212,139,225,15,138,30,18,253,150,188,117,251,105,232,158,32,241,22,171,167,120,182,226,206,231,192,30,37,253,153,252,39,225,207,18,232,151,90,77,198,169,169,65,255,0,11,83,65,209,188,71,165,104,26,253,183,138,52,175,13,255,0,158,175,252,19,178,251,246,180,253,150,252,17,240,239,226,159,236,123,241,115,196,95,5,124,83,241,91,246,114,248,75,105,241,2,125,19,70,240,159,139,60,63,227,61,22,47,14,248,127,91,240,247,252,36,158,8,248,129,225,93,111,69,212,181,205,50,234,239,80,125,39,85,151,78,109,83,74,131,197,154,213,174,153,123,103,103,174,106,246,250,135,232,38,171,251,74,255,0,193,66,53,27,63,12,235,118,158,38,240,94,133,241,199,65,253,165,238,127,107,77,79,246,150,210,190,29,88,205,241,87,198,127,25,46,63,102,13,119,246,49,77,79,196,190,18,215,45,111,62,29,90,232,145,126,204,154,182,153,225,104,236,52,127,2,105,48,148,240,150,159,173,75,230,120,146,93,87,89,212,250,231,152,82,92,169,75,150,74,75,153,53,123,173,157,181,244,176,148,27,94,167,247,213,69,127,8,255,0,240,223,191,240,90,207,250,61,29,115,255,0,17,247,246,86,255,0,232,117,163,254,27,247,254,11,89,255,0,71,163,174,127,226,62,254,202,223,253,14,181,95,218,88,95,230,127,119,252,31,48,228,145,253,220,81,95,194,63,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,218,88,95,230,127,119,252,31,48,228,145,253,220,81,95,194,63,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,218,88,95,230,127,119,252,31,48,228,145,253,220,81,95,194,63,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,218,88,95,230,127,119,252,31,48,228,145,253,220,81,95,194,63,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,218,88,87,246,159,221,255,0,4,57,36,127,119,20,87,240,143,255,0,13,251,255,0,5,172,255,0,163,209,215,63,241,31,127,101,111,254,135,90,241,127,138,95,240,80,159,248,45,5,175,142,63,103,8,110,127,109,207,28,219,77,168,124,103,215,108,244,248,116,143,132,223,179,239,135,236,53,11,196,253,158,62,61,106,15,103,226,125,35,65,248,31,107,107,227,157,13,108,44,111,167,143,74,214,224,212,180,200,117,59,45,59,91,138,202,61,103,71,210,53,27,10,134,97,133,155,73,79,87,255,0,14,46,70,143,244,32,162,191,146,111,217,251,254,14,28,253,160,124,31,241,31,66,209,127,111,15,128,223,13,83,225,63,138,181,157,35,77,191,248,169,251,58,104,159,16,116,29,83,224,230,150,246,126,32,58,143,138,188,75,240,235,197,30,47,241,124,255,0,22,116,70,212,147,66,55,22,250,46,161,164,235,26,86,159,166,234,87,86,58,87,138,239,174,44,116,149,254,162,62,10,252,106,248,85,251,70,124,42,240,63,198,239,130,62,56,209,62,35,252,43,248,143,162,69,175,248,59,198,58,4,179,53,142,169,98,211,77,103,117,111,113,107,121,12,87,90,54,183,101,169,218,95,88,234,122,101,244,22,218,150,149,169,105,183,122,110,165,105,105,127,105,115,109,23,77,58,212,235,46,106,115,82,66,105,167,102,126,7,255,0,193,55,190,4,190,175,255,0,4,239,253,130,245,111,176,6,254,212,253,139,255,0,101,189,68,62,216,206,241,123,240,63,192,215,33,206,79,57,18,231,159,90,95,3,124,9,121,127,224,161,255,0,181,38,147,246,1,155,47,216,191,246,10,212,76,100,70,118,141,83,227,143,252,20,130,216,55,45,212,157,32,142,14,126,78,120,175,171,63,224,151,159,19,116,11,15,248,38,127,252,19,182,198,101,182,243,172,191,97,127,217,34,210,82,209,150,111,54,219,224,15,195,248,92,147,180,252,219,208,215,7,225,221,124,234,159,240,86,175,142,158,60,176,248,161,227,47,11,120,103,194,127,176,191,236,53,103,226,31,133,186,14,151,240,206,231,192,159,23,164,213,254,50,255,0,193,79,44,180,207,248,88,87,254,40,248,111,169,120,155,76,58,5,221,218,106,26,79,252,34,126,34,240,191,159,121,112,209,235,223,219,122,120,142,198,63,207,233,78,111,27,141,187,113,73,206,207,95,249,249,30,199,99,218,31,35,242,87,254,9,113,251,58,65,227,143,217,155,246,81,212,37,211,214,81,119,251,26,252,14,212,217,228,183,83,185,238,254,30,124,54,125,224,8,220,21,216,233,131,180,19,146,73,57,175,212,159,248,99,171,47,250,4,197,255,0,128,170,57,244,201,176,3,211,191,122,249,27,254,9,33,226,3,103,251,49,248,22,120,126,41,248,203,195,182,159,26,191,99,111,132,186,151,195,125,3,77,210,254,21,92,232,255,0,177,229,182,181,240,251,65,125,63,194,95,179,141,199,138,126,29,106,23,254,39,208,180,40,252,73,161,193,166,73,241,110,255,0,226,150,169,44,95,14,180,86,214,181,13,94,107,143,17,77,175,126,155,235,41,226,109,79,224,223,132,254,24,216,254,216,255,0,31,124,59,227,111,14,107,247,26,206,177,251,69,104,222,18,253,140,167,248,201,227,221,58,121,252,77,44,94,19,241,102,135,226,31,217,94,255,0,225,245,158,129,12,122,246,151,20,114,232,94,4,209,117,82,158,11,211,12,218,156,178,203,172,73,171,44,83,190,34,167,251,71,47,188,254,204,180,215,209,245,254,153,48,94,234,188,111,111,248,7,128,255,0,195,28,217,231,31,217,17,127,224,42,243,255,0,146,30,180,127,195,28,217,127,208,38,32,6,50,77,162,244,57,255,0,167,15,241,175,175,117,159,23,190,167,241,147,194,127,19,172,126,55,248,251,195,158,9,240,239,135,238,116,109,99,246,117,209,180,79,128,179,252,27,241,238,163,60,30,37,138,47,22,120,179,92,241,15,194,27,255,0,136,54,126,32,134,77,119,75,150,56,244,47,29,232,186,81,147,193,122,96,155,76,150,41,117,136,245,110,127,68,213,181,77,43,254,23,159,219,255,0,105,191,139,190,37,255,0,133,181,253,175,255,0,8,15,246,223,134,255,0,101,184,63,225,152,63,180,255,0,225,40,251,31,252,40,207,248,71,63,103,205,63,254,18,15,236,255,0,248,72,52,159,178,127,194,203,255,0,133,135,191,254,16,125,47,237,255,0,109,243,181,175,237,142,123,171,105,137,255,0,201,101,229,229,253,126,5,89,127,39,245,167,245,255,0,14,124,197,255,0,12,117,101,255,0,64,152,191,240,21,71,62,153,54,0,122,119,239,71,252,49,205,158,113,253,145,23,254,2,175,63,249,33,235,94,253,172,167,137,181,63,131,126,19,248,99,99,251,99,252,125,240,239,141,188,57,175,220,107,58,199,237,21,163,120,75,246,50,159,227,39,143,116,233,231,241,52,177,120,79,197,154,31,136,127,101,123,255,0,135,214,122,4,49,235,218,92,81,203,161,120,19,69,213,74,120,47,76,51,106,114,203,46,177,38,173,223,235,62,47,125,79,227,39,132,254,39,88,252,111,241,247,135,60,19,225,223,15,220,232,218,199,236,235,163,104,159,1,103,248,55,227,221,70,120,60,75,20,94,44,241,102,185,226,31,132,55,255,0,16,108,252,65,12,154,238,151,44,113,232,94,59,209,116,163,39,130,244,193,54,153,44,82,235,17,234,197,215,76,79,254,75,47,43,116,235,211,241,220,44,191,147,250,211,250,255,0,135,62,66,255,0,134,57,178,255,0,160,76,64,12,100,155,69,232,115,255,0,78,31,227,71,252,49,213,151,253,2,98,255,0,192,85,28,250,100,216,1,233,223,189,125,59,162,106,218,166,149,255,0,11,207,237,255,0,180,223,197,223,18,255,0,194,218,254,215,255,0,132,7,251,111,195,127,178,220,31,240,204,31,218,127,240,148,125,143,254,20,103,252,35,159,179,230,159,255,0,9,7,246,127,252,36,26,79,217,63,225,101,255,0,194,195,223,255,0,8,62,151,246,255,0,182,249,218,215,246,198,6,178,158,38,212,254,13,248,79,225,141,143,237,143,241,247,195,190,54,240,230,191,113,172,235,31,180,86,141,225,47,216,202,127,140,158,61,211,167,159,196,210,197,225,63,22,104,126,33,253,149,239,254,31,89,232,16,199,175,105,113,71,46,133,224,77,23,85,41,224,189,48,205,169,203,44,186,196,154,177,117,162,88,174,223,102,94,94,93,63,171,133,151,242,127,90,127,95,240,231,128,255,0,195,28,217,231,31,217,17,127,224,42,243,255,0,146,30,180,127,195,28,217,127,208,38,32,6,50,77,162,244,57,255,0,167,15,241,175,175,117,159,23,190,167,241,147,194,127,19,172,126,55,248,251,195,158,9,240,239,135,238,116,109,99,246,117,209,180,79,128,179,252,27,241,238,163,60,30,37,138,47,22,120,179,92,241,15,194,27,255,0,136,54,126,32,134,77,119,75,150,56,244,47,29,232,186,81,147,193,122,96,155,76,150,41,117,136,245,110,127,68,213,181,77,43,254,23,159,219,255,0,105,191,139,190,37,255,0,133,181,253,175,255,0,8,15,246,223,134,255,0,101,184,63,225,152,63,180,255,0,225,40,251,31,252,40,207,248,71,63,103,205,63,254,18,15,236,255,0,248,72,52,159,178,127,194,203,255,0,133,135,191,254,16,125,47,237,255,0,109,243,181,175,237,130,234,218,98,127,242,89,121,121,127,95,128,89,127,39,245,167,245,255,0,14,124,197,255,0,12,117,101,255,0,64,152,191,240,21,71,62,153,54,0,122,119,239,71,252,49,205,151,67,164,68,62,182,171,244,255,0,159,15,90,247,237,101,60,77,169,252,27,240,159,195,27,31,219,31,227,239,135,60,109,225,205,126,231,89,214,63,104,173,27,194,95,177,148,255,0,25,60,123,167,79,63,137,165,139,194,126,44,208,252,67,251,43,95,252,62,179,208,33,143,94,210,226,142,93,11,192,154,46,170,83,193,122,97,155,83,150,89,117,137,53,110,247,91,241,108,154,175,198,47,10,124,77,176,248,229,241,3,195,62,10,240,239,135,238,52,109,95,246,119,209,52,63,128,87,31,7,124,119,168,205,15,137,34,143,197,158,43,214,252,73,240,127,80,248,129,103,226,8,95,93,210,228,142,61,15,199,90,54,148,95,193,154,104,151,76,150,57,117,136,245,84,229,101,166,35,153,173,151,44,149,237,109,54,235,211,241,176,89,127,47,245,253,127,90,159,34,127,195,29,89,31,249,132,69,255,0,128,171,207,254,72,87,199,159,180,231,236,173,103,163,124,99,255,0,130,125,91,255,0,101,32,93,119,246,188,241,198,151,34,45,180,96,202,150,191,176,23,237,197,226,34,159,61,154,169,231,65,86,249,142,220,160,39,0,100,126,235,255,0,194,243,240,159,252,250,232,223,247,197,183,255,0,36,87,195,159,181,159,197,191,12,107,31,29,255,0,224,153,66,56,116,216,227,211,191,109,191,136,218,133,203,64,177,43,121,3,254,9,163,255,0,5,14,178,195,152,231,63,39,157,123,15,222,249,114,6,236,174,65,120,74,213,157,117,205,179,82,255,0,210,24,56,198,219,118,252,209,249,63,251,83,254,202,250,61,134,143,168,67,54,158,159,119,200,101,156,236,72,145,12,27,157,84,76,30,34,178,168,104,228,95,59,6,240,201,24,14,10,159,71,255,0,131,120,255,0,104,15,136,254,14,253,160,126,60,126,193,250,214,187,173,120,175,225,50,252,53,214,255,0,104,191,133,118,26,150,175,102,250,95,193,189,83,65,248,131,225,15,11,124,69,240,207,133,116,255,0,248,71,197,204,186,47,139,47,190,46,248,123,90,184,183,23,214,218,118,149,172,120,79,85,190,181,211,110,47,188,87,171,95,15,171,191,107,127,26,248,120,216,106,198,21,130,49,56,150,40,133,171,219,150,158,77,147,68,20,35,249,69,246,188,193,20,174,240,68,45,140,109,218,127,152,251,175,216,107,226,207,252,21,15,246,153,214,63,103,111,128,30,34,248,117,225,15,27,120,79,225,255,0,136,126,51,234,90,167,198,29,87,196,122,15,133,102,240,191,134,188,79,225,47,7,95,105,246,87,190,11,240,151,136,110,229,215,223,83,248,163,225,247,134,41,44,98,183,104,44,175,89,238,163,146,56,98,184,247,178,74,245,92,226,146,186,123,249,232,99,85,37,125,54,63,92,63,96,223,218,90,195,66,253,134,255,0,99,29,13,238,229,87,209,191,101,31,217,219,73,101,13,22,21,180,239,132,30,15,179,101,25,184,4,0,97,199,32,125,41,124,31,251,75,88,69,251,114,126,209,58,231,218,230,242,245,31,217,71,246,50,210,149,179,22,230,125,27,226,247,237,229,120,203,255,0,31,28,128,53,212,199,251,252,133,239,252,225,126,201,95,180,247,196,191,21,126,205,218,124,159,10,254,24,252,72,248,155,225,47,217,143,224,135,195,231,248,221,226,175,135,190,18,241,71,140,188,53,240,123,195,186,31,128,238,217,252,65,241,79,91,240,214,135,117,107,240,243,67,26,111,129,252,87,114,110,181,105,173,109,197,191,134,117,25,188,207,46,202,229,163,241,253,39,254,10,69,224,219,79,141,30,63,241,179,120,139,72,22,190,32,248,95,240,135,194,208,204,117,235,81,20,151,30,15,241,95,198,253,90,230,36,152,174,29,210,63,28,218,150,81,202,137,208,158,28,86,177,203,106,123,124,68,148,62,59,254,50,139,254,174,37,61,21,228,126,199,254,197,191,181,109,159,195,127,217,203,246,98,180,184,213,96,181,123,47,217,127,224,206,130,67,93,44,108,141,99,240,247,193,41,176,177,157,48,12,22,246,223,40,111,249,101,202,2,185,175,175,191,225,224,26,103,253,12,54,255,0,248,48,79,254,88,215,7,255,0,4,5,255,0,130,52,107,31,181,87,193,127,3,254,211,31,183,159,195,123,235,63,217,151,87,248,9,224,223,8,254,206,127,7,181,79,17,252,83,248,113,241,23,226,62,162,44,188,25,56,253,163,239,53,79,135,126,55,208,111,252,43,240,200,104,186,30,169,166,248,94,202,252,222,31,24,218,248,178,127,19,67,105,166,232,22,30,20,213,124,87,247,45,159,252,19,131,254,9,235,168,254,210,94,25,248,111,167,126,200,154,13,255,0,194,15,25,127,193,78,124,113,251,22,248,75,199,182,95,180,47,237,183,117,255,0,9,79,195,15,134,159,240,75,95,29,126,212,95,16,245,207,14,120,166,15,218,137,180,189,111,226,6,131,251,107,252,42,248,171,240,235,91,186,182,130,93,63,75,255,0,133,105,175,248,70,251,74,183,241,110,141,168,234,22,219,212,201,227,57,115,212,178,117,37,109,155,122,189,47,109,189,68,167,100,146,232,124,173,255,0,15,0,211,63,232,97,183,255,0,193,130,127,242,198,143,248,120,6,153,255,0,67,13,191,254,12,19,255,0,150,53,251,221,255,0,14,7,255,0,130,79,127,209,178,248,139,255,0,18,115,246,183,255,0,231,239,71,252,56,31,254,9,61,255,0,70,203,226,47,252,73,207,218,223,255,0,159,189,55,144,193,253,165,248,249,7,180,245,63,4,127,225,224,26,103,253,12,54,255,0,248,48,79,254,88,209,255,0,15,0,211,63,232,97,183,255,0,193,130,127,242,198,191,123,191,225,192,255,0,240,73,239,250,54,95,17,127,226,78,126,214,255,0,252,253,232,255,0,135,3,255,0,193,39,191,232,217,124,69,255,0,137,57,251,91,255,0,243,247,161,228,48,127,105,126,62,65,237,61,79,193,31,248,120,6,153,255,0,67,13,191,254,12,19,255,0,150,52,127,195,192,52,207,250,24,109,255,0,240,96,159,252,177,175,222,239,248,112,63,252,18,123,254,141,151,196,95,248,147,159,181,191,255,0,63,122,63,225,192,255,0,240,73,239,250,54,95,17,127,226,78,126,214,255,0,252,253,232,121,12,31,218,95,143,144,123,79,83,240,71,254,30,1,166,127,208,195,111,255,0,131,4,255,0,229,141,31,240,240,13,51,254,134,27,127,252,24,39,255,0,44,107,247,187,254,28,15,255,0,4,158,255,0,163,101,241,23,254,36,231,237,111,255,0,207,222,143,248,112,63,252,18,123,254,141,151,196,95,248,147,159,181,191,255,0,63,122,30,67,7,246,151,227,228,30,211,212,252,17,255,0,135,128,105,159,244,48,219,255,0,224,193,63,249,99,71,252,60,3,76,255,0,161,134,223,255,0,6,9,255,0,203,26,253,238,255,0,135,3,255,0,193,39,191,232,217,124,69,255,0,137,57,251,91,255,0,243,247,163,254,28,15,255,0,4,158,255,0,163,101,241,23,254,36,231,237,111,255,0,207,222,135,144,193,253,165,248,249,7,180,245,63,4,127,225,224,26,103,253,12,54,255,0,248,48,79,254,88,215,134,124,77,253,182,172,60,69,241,159,246,45,187,143,90,134,127,248,69,127,104,255,0,23,248,129,194,93,172,198,53,159,246,58,253,171,252,49,230,21,23,18,224,111,241,34,46,118,113,230,117,25,175,233,135,254,28,15,255,0,4,158,255,0,163,101,241,23,254,36,231,237,111,255,0,207,222,190,104,253,163,63,224,220,15,216,107,226,46,171,251,61,47,192,175,10,248,191,224,21,167,131,254,55,106,94,43,248,205,227,79,14,252,123,248,197,226,111,136,55,223,10,100,248,1,241,219,194,54,126,28,248,111,167,124,106,190,241,215,135,23,196,147,252,98,241,103,194,75,139,201,111,180,155,87,95,15,105,90,224,180,212,161,188,104,45,111,52,167,146,198,156,148,185,147,178,107,175,85,97,58,151,232,126,6,252,122,253,180,103,241,156,154,87,132,60,42,117,31,24,248,195,198,218,174,139,225,175,8,120,71,195,150,23,222,32,241,31,137,188,79,175,93,166,149,161,120,127,195,250,46,138,243,93,248,155,93,186,188,132,65,103,166,219,37,197,229,244,247,235,107,109,3,73,34,238,254,137,191,224,133,95,240,79,127,16,254,205,95,9,117,255,0,218,155,227,199,135,60,75,225,223,218,107,246,156,211,45,30,227,192,62,61,240,215,133,180,207,17,124,1,248,69,167,107,186,181,255,0,134,252,9,103,45,148,115,106,90,55,136,188,82,173,161,248,159,197,246,87,55,54,18,67,113,109,225,207,15,234,154,21,158,175,224,249,238,175,62,243,253,154,63,224,151,31,176,71,236,133,227,41,126,34,252,4,253,156,252,53,225,191,136,35,103,246,95,142,188,89,226,79,31,252,93,241,151,132,243,165,107,218,21,247,252,43,255,0,19,252,99,241,110,191,123,240,227,237,250,39,137,117,139,61,87,251,6,125,55,251,98,214,120,237,245,79,182,69,109,106,144,253,249,93,216,60,190,150,18,237,89,205,254,2,148,156,189,15,231,243,254,11,93,23,156,191,24,87,118,210,159,240,64,127,248,47,116,192,227,60,195,39,252,19,138,77,184,200,234,1,25,237,156,243,210,191,160,58,40,174,229,24,169,74,73,90,82,181,252,237,177,1,69,20,85,0,81,69,20,0,81,69,20,0,81,69,20,0,81,69,20,0,81,69,20,0,81,69,20,0,81,69,20,1,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 5515; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

