#ifndef GETVESUITE_Pipeline_Pipeline_V-PIPE_H
#define GETVESUITE_Pipeline_Pipeline_V-PIPE_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Pipeline_Pipeline_V-PIPE( void )
{
    unsigned char osgData[ 3979 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,110,0,37,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,253,194,255,0,130,157,127,193,105,117,79,131,222,56,241,39,236,221,251,25,106,223,14,239,124,115,224,75,31,22,197,251,69,126,209,126,53,142,215,92,248,111,240,18,109,51,71,214,44,53,47,13,120,85,101,213,237,180,205,95,226,167,135,181,5,26,158,189,168,107,45,123,225,143,11,205,225,245,240,254,169,164,235,250,173,206,189,99,225,95,227,51,246,165,255,0,130,241,175,196,219,175,20,88,120,227,226,247,237,77,251,100,218,120,155,89,240,220,126,44,240,126,181,227,219,207,129,31,179,79,138,252,63,165,248,123,78,187,211,110,237,62,21,232,250,121,209,160,214,52,175,20,104,254,30,127,178,55,195,189,54,11,173,83,77,184,215,126,221,45,252,113,223,234,63,60,248,179,66,248,147,251,104,254,202,250,134,131,166,248,182,123,47,142,30,58,188,208,126,56,248,130,63,22,248,170,255,0,80,215,62,58,223,234,73,174,248,131,82,210,60,91,227,43,233,34,109,91,89,215,252,77,171,141,109,53,29,94,75,139,123,175,18,120,118,217,117,41,109,167,50,234,154,127,224,191,138,188,37,226,175,3,107,183,222,23,241,183,134,124,65,224,255,0,19,105,130,209,181,47,14,248,167,70,212,124,63,174,233,235,127,99,107,169,216,181,238,145,171,91,195,113,106,39,211,111,108,238,34,50,70,190,108,23,113,204,155,163,145,88,241,83,75,17,58,158,210,163,92,174,220,137,218,201,117,118,213,223,238,41,251,169,89,111,212,253,159,211,191,224,173,95,7,124,57,168,216,120,131,193,191,177,102,163,224,207,22,232,87,182,154,207,133,188,99,225,127,218,48,104,62,37,240,167,137,52,201,227,189,208,252,77,225,221,111,71,248,25,111,121,163,107,182,26,165,189,181,213,157,221,165,197,189,205,181,197,172,114,219,205,20,168,178,47,238,127,236,89,255,0,5,246,248,253,125,174,93,248,123,246,114,253,171,190,36,248,251,92,182,212,252,83,226,107,47,217,235,246,227,208,127,225,104,222,248,254,214,219,193,154,125,134,161,168,216,120,254,111,24,223,120,149,172,45,89,162,212,236,188,61,160,124,64,211,158,41,188,35,169,106,83,104,143,167,201,170,75,168,255,0,14,39,252,254,63,254,175,214,190,178,253,148,191,102,47,140,95,30,124,115,161,234,190,6,125,111,192,30,18,240,206,181,253,165,174,124,118,146,211,88,177,240,239,195,251,175,12,190,147,171,77,46,147,174,216,188,7,81,248,131,9,212,180,73,52,221,42,202,234,59,247,184,212,109,46,36,146,199,79,91,141,74,210,165,66,157,56,185,194,163,162,227,215,153,181,211,116,219,79,229,168,115,54,245,87,63,217,111,246,3,253,190,254,17,254,223,223,8,63,225,60,240,42,31,8,124,70,240,153,211,180,143,141,127,4,181,141,86,215,82,241,119,194,111,22,95,71,122,45,98,158,230,27,123,115,226,79,1,234,173,165,234,179,248,107,196,113,90,219,91,235,54,150,23,17,77,107,165,235,154,110,187,161,105,5,127,151,95,237,43,255,0,5,8,248,225,251,47,252,71,186,184,253,152,254,42,252,93,248,55,109,241,10,27,135,241,117,239,192,255,0,140,222,54,248,75,31,138,191,225,20,212,175,173,244,43,63,17,63,129,228,179,255,0,132,145,52,171,141,111,196,105,102,110,76,171,100,250,197,242,64,177,52,215,1,138,210,149,73,206,156,38,225,172,151,116,191,49,73,89,180,125,211,255,0,4,253,248,67,224,207,140,31,3,62,19,232,222,43,183,212,32,185,181,240,15,130,227,208,252,71,160,234,19,104,254,37,240,221,206,171,225,91,123,25,175,116,157,66,28,164,184,38,214,115,105,123,13,222,155,113,113,166,90,73,121,99,115,246,104,149,127,104,103,255,0,130,75,248,203,227,31,133,229,179,208,62,40,124,51,248,187,161,217,234,186,111,138,60,43,240,159,246,167,248,89,165,248,159,65,155,95,75,84,240,245,246,171,174,248,255,0,74,182,191,181,211,175,109,172,53,63,17,207,97,61,167,129,100,144,199,58,233,50,178,45,205,206,171,95,149,191,240,74,175,249,37,31,9,255,0,236,73,248,111,255,0,166,75,58,254,197,191,101,207,248,242,211,255,0,235,221,187,15,249,250,95,106,248,252,199,21,94,133,105,202,156,236,249,159,230,191,166,116,83,73,218,253,191,200,252,36,215,255,0,224,129,95,23,52,104,237,19,197,95,5,63,224,151,122,46,133,169,24,226,213,53,223,10,252,54,79,17,235,218,30,153,35,162,95,234,218,87,135,239,191,102,141,18,45,111,84,182,181,146,73,173,236,159,86,211,99,187,150,37,130,77,66,202,55,55,49,249,103,199,95,216,150,79,11,218,106,18,124,86,248,213,227,63,138,87,127,218,115,188,250,118,133,96,62,26,120,71,81,177,109,2,75,43,72,181,13,58,207,87,213,117,161,169,67,121,44,247,6,226,211,196,54,112,202,209,91,70,246,123,99,185,55,159,216,255,0,198,158,124,59,6,121,255,0,137,117,215,95,250,224,191,210,191,156,255,0,219,43,136,245,31,121,199,254,145,201,143,167,90,228,195,99,241,53,229,15,105,59,244,252,138,148,18,73,173,63,175,248,7,240,59,255,0,5,45,181,181,177,248,153,224,219,27,27,107,123,43,43,45,7,82,180,179,179,180,134,59,123,91,91,91,107,171,56,109,237,237,173,225,80,144,64,144,162,42,34,128,170,168,2,128,5,21,63,252,20,223,254,74,183,133,191,236,19,172,127,233,117,173,21,247,120,111,224,82,244,71,52,254,38,127,64,159,240,74,175,249,37,31,9,255,0,236,73,248,111,255,0,166,75,58,254,197,191,101,207,248,242,211,255,0,235,221,191,244,169,107,248,233,255,0,130,85,103,254,21,71,194,115,142,63,225,9,248,111,248,255,0,196,150,200,96,99,223,143,169,252,255,0,177,111,217,115,254,60,180,255,0,250,247,111,253,42,90,248,140,221,90,172,239,252,207,244,58,41,116,244,255,0,35,239,31,141,31,242,46,193,255,0,96,235,175,253,16,181,252,231,254,217,95,234,245,31,250,238,63,244,138,74,254,140,62,52,127,200,187,7,253,131,174,191,244,66,215,243,159,251,101,127,171,212,127,235,184,255,0,210,41,43,207,192,124,112,245,253,75,158,203,212,254,9,63,224,166,255,0,242,85,188,45,255,0,96,157,99,255,0,75,173,104,163,254,10,111,255,0,37,91,194,223,246,9,214,125,127,231,254,215,142,148,87,232,248,111,247,122,95,225,71,36,254,38,126,202,255,0,193,50,181,63,16,124,32,248,93,240,211,85,211,124,29,226,95,31,124,54,212,252,49,224,221,119,91,211,124,23,103,105,171,248,235,225,222,165,169,90,71,175,120,159,196,54,94,28,154,246,27,223,136,158,11,185,146,227,89,212,37,211,116,116,212,252,91,107,169,202,44,116,13,23,196,150,186,181,174,159,225,175,235,219,246,40,253,175,191,102,79,30,248,167,194,255,0,11,52,95,141,126,5,211,126,50,107,176,234,43,167,124,6,241,214,168,126,24,254,208,164,233,150,119,158,34,186,23,191,0,62,36,195,164,248,203,73,207,133,52,219,173,106,31,181,232,112,139,157,8,71,174,91,121,218,76,240,94,73,252,188,127,193,42,143,252,90,143,132,255,0,246,36,252,56,255,0,211,37,157,127,91,223,9,62,24,252,54,248,201,240,246,235,225,183,197,239,135,190,7,248,171,240,239,196,150,214,11,226,47,0,252,73,240,158,131,227,159,5,235,195,72,215,244,253,119,74,26,215,133,188,81,97,117,99,170,11,109,115,76,211,111,45,252,248,36,242,110,180,248,46,35,219,52,81,186,252,126,107,58,82,173,83,218,197,252,79,88,218,253,53,105,232,254,245,250,155,211,190,150,215,79,242,63,68,63,104,191,18,120,119,193,223,15,53,127,23,120,187,94,209,188,43,225,79,10,248,83,94,241,31,137,252,79,226,61,82,199,67,240,239,135,60,61,161,233,114,234,122,222,189,175,107,122,156,241,91,105,26,45,158,157,109,117,113,117,117,113,36,112,91,193,108,242,205,34,70,172,195,249,72,253,172,127,107,255,0,130,190,62,142,254,63,129,58,190,169,251,76,205,52,193,45,181,15,217,231,78,79,136,223,15,30,250,27,73,38,212,180,45,75,227,205,189,221,191,195,175,10,248,174,211,73,198,161,62,141,173,120,187,77,213,141,157,213,139,91,217,79,54,173,163,193,168,127,64,126,56,255,0,130,113,255,0,193,60,254,30,63,134,252,105,224,15,216,59,246,50,240,55,140,124,45,119,105,226,111,12,248,179,193,255,0,178,247,193,15,12,248,155,195,190,36,208,174,45,181,77,15,196,26,22,187,162,248,26,11,173,35,91,178,212,237,173,174,109,46,237,229,142,226,222,123,116,154,25,18,68,86,31,147,159,182,87,250,189,71,254,187,129,255,0,146,114,31,231,92,24,39,134,82,131,140,103,55,126,182,138,251,149,239,247,162,231,123,46,154,159,231,243,255,0,5,15,210,245,171,127,138,94,31,213,252,83,61,140,222,38,214,116,123,243,168,91,105,13,36,154,38,135,103,107,126,175,167,120,115,71,185,184,180,130,125,90,210,209,175,111,12,154,141,212,49,92,106,23,87,151,23,66,219,78,180,123,77,35,78,43,171,255,0,130,155,255,0,201,86,240,183,253,130,117,143,210,250,212,15,211,249,81,95,160,97,221,232,82,118,181,210,57,103,241,51,250,4,255,0,130,85,127,201,40,248,79,255,0,98,79,195,127,253,50,89,215,246,45,251,46,127,199,150,159,255,0,94,237,255,0,165,75,95,199,79,252,18,171,254,73,71,194,127,251,18,126,27,255,0,233,146,206,191,177,111,217,115,254,60,180,255,0,250,247,111,253,42,90,248,124,223,248,179,255,0,19,253,14,138,93,61,63,200,251,199,227,71,252,139,176,127,216,58,235,255,0,68,45,127,57,255,0,182,87,250,189,71,254,187,143,253,34,146,191,163,15,141,31,242,46,193,255,0,96,235,175,253,16,181,252,231,254,217,95,234,245,31,250,238,63,244,138,74,243,240,31,28,61,127,82,231,178,245,63,130,79,248,41,191,252,149,111,11,127,216,39,88,255,0,210,235,90,40,255,0,130,155,255,0,201,86,240,183,253,130,117,143,253,46,181,162,191,71,195,127,187,210,255,0,10,57,39,241,51,250,4,255,0,130,85,127,201,40,248,79,255,0,98,79,195,127,253,50,89,215,246,45,251,46,127,199,150,159,255,0,94,237,255,0,165,75,95,199,79,252,18,171,254,73,79,194,126,223,241,68,252,55,235,255,0,96,75,60,226,191,177,111,217,115,254,60,180,255,0,250,247,111,253,42,90,248,140,223,248,179,255,0,19,253,14,138,93,61,63,200,251,199,227,71,252,139,176,127,216,58,235,255,0,68,45,127,57,255,0,182,87,250,189,71,254,187,143,253,34,146,191,163,15,141,63,242,46,193,255,0,96,235,175,214,5,199,95,243,205,127,57,255,0,182,87,250,189,71,167,250,241,248,255,0,161,73,211,29,235,207,192,252,112,245,255,0,34,231,177,252,18,127,193,77,255,0,228,171,120,91,254,193,58,199,254,151,90,209,71,252,20,220,19,241,91,194,216,4,255,0,196,167,88,233,207,252,191,90,156,123,28,99,243,162,191,71,195,53,236,41,107,209,28,147,248,153,253,41,124,19,240,44,127,177,111,199,175,137,223,178,95,138,141,236,122,135,236,253,241,31,90,248,79,166,106,30,33,210,117,15,6,234,126,43,240,223,130,53,123,141,55,192,30,57,255,0,132,87,83,146,242,125,15,71,241,95,129,161,208,124,67,164,137,47,111,98,159,76,241,13,164,246,151,250,149,180,177,94,77,253,51,254,205,159,25,252,43,97,97,97,44,183,182,209,193,29,185,121,37,222,101,18,70,206,178,47,148,28,196,86,65,33,85,32,130,9,200,7,114,184,79,189,63,110,191,248,37,175,236,191,255,0,5,2,213,126,31,120,183,226,202,120,231,192,191,19,126,27,9,180,221,15,226,215,193,221,87,195,94,26,248,129,168,248,50,224,106,87,47,240,235,197,23,190,39,240,142,183,99,226,95,4,199,174,234,47,170,89,65,119,167,201,119,165,95,155,167,210,47,44,32,214,53,251,125,91,240,167,226,207,252,18,223,246,247,253,144,252,29,241,119,226,198,149,241,211,246,126,248,177,240,67,224,135,128,252,73,241,103,80,214,245,105,190,36,252,55,248,177,226,15,10,252,56,240,20,190,48,241,30,149,101,240,242,195,194,126,32,210,44,53,168,238,180,173,110,223,77,89,252,91,115,21,247,238,46,239,46,108,13,204,182,150,158,14,101,148,212,173,39,58,126,242,111,186,91,216,184,84,229,183,161,251,159,241,31,227,247,195,205,103,195,182,80,139,235,57,46,39,180,145,34,62,116,197,92,61,186,156,40,73,99,221,1,222,153,108,238,28,99,27,190,95,193,175,219,19,226,47,128,219,79,212,110,102,123,84,85,6,71,146,75,251,184,200,17,35,35,92,8,213,27,229,117,97,18,198,9,109,217,4,121,170,234,191,147,94,2,255,0,130,133,124,80,248,247,241,15,225,183,192,191,4,233,169,166,120,211,227,79,196,207,135,31,9,60,33,169,120,175,88,184,181,240,190,157,226,143,137,30,55,208,60,25,225,235,255,0,17,221,104,214,87,87,118,186,28,58,190,183,105,37,220,150,214,183,83,199,111,20,143,21,173,204,138,176,73,251,215,240,223,254,8,5,241,67,226,242,120,138,227,246,236,253,166,6,147,167,221,54,163,109,160,120,15,246,74,184,223,44,82,201,7,135,205,143,138,117,95,138,191,24,62,30,157,225,228,62,43,138,227,66,143,193,236,203,35,88,223,195,226,64,141,119,165,55,159,130,201,171,115,167,162,138,243,94,95,240,77,39,83,68,158,135,224,23,236,163,255,0,4,58,240,247,252,22,123,197,159,27,252,103,125,241,51,82,248,7,225,47,129,26,151,131,252,45,161,248,191,254,21,119,136,190,42,120,115,199,222,36,241,178,120,167,88,241,87,131,172,117,120,62,40,120,86,215,75,241,23,134,52,173,15,193,151,90,141,162,203,168,93,173,191,196,237,46,107,168,108,35,150,209,175,138,255,0,67,47,131,31,6,62,22,254,207,31,11,124,21,240,87,224,175,130,180,111,135,159,11,254,30,104,201,161,120,75,194,90,18,79,246,61,58,207,207,158,246,242,234,234,242,246,121,174,181,189,118,247,85,187,191,191,213,53,75,249,238,117,45,91,82,212,238,245,61,78,238,238,254,238,230,230,82,190,194,149,37,78,156,41,167,126,85,109,217,206,219,110,231,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 3979; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

