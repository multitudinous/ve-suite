import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.lang.String;

class ObserverServant extends _ObserverStub{

   JFrame main_frame;

   public ObserverServant(JFrame f)
   {
      main_frame = f;
      my_baf = new short[15];
      obj_chg = false;
   }

   public int update()
   {
      System.out.println("ObserverServant: Updating GUI");
      return id;	
   }

   public int get_iso_value()
   {
      System.out.println("ObserverServant: iso value:"+iso_value);
      return iso_value;
   }

   public int get_sc()
   {
      System.out.println("ObserverServant: scalar: "+sc);
      return sc;
   }

   public int get_min()
   {
      System.out.println("ObserverServant: minimum value:"+min);
      return min;
   }

   public int get_max()
   {
      //System.out.println("ObserverServant: maximum value:"+max);
      return max;
   }

   public int get_geo_state()
   {
      //System.out.println("ObserverServant: geometry state:"+geo_state);
      return geo_state;
   }

   public int get_pre_state()
   {
      //System.out.println("ObserverServant: pre-processing state:"+pre_state);
      return pre_state;
   }

   public short[] get_baf_param()
   {
      //System.out.println("ObserverServant: send baffle parameters.");
      return my_baf;
   }

   public short[] get_other_param()
   {
      //System.out.println("ObserverServant: send other parameters.");
      return my_param_val;
   }

   public void convert_p(short[] p)
   {
      for(int i = 0; i < 15; i++)
         my_baf[i] = p[i];
   }

   public void set_param(int[] p)
   {
      my_param_val = new short[p.length];

      for(int i = 0; i < p.length; i++)
         my_param_val[i] = (short)p[i];
   } 

   public void ack(String d)
   {
      System.out.println(d);
      ack_win awin = new ack_win(d,1,main_frame);
      awin.start();
   }

   public void put_cur_obj(short[] o)
   {
      cur_obj = o;
      obj_chg = true;
      System.out.println("************");
   }

   public short get_timesteps()
   {
      System.out.println("ObserverServant: send timesteps "+timesteps);
      return timesteps;
   }

   public short get_teacher_state()
   {
      //System.out.println("ObserverServant: send teacher_state "+teacher_state);
      return teacher_state;
   }
// these are the variables that can be returned to cfdApp
   public int id; 
   public int iso_value;
   public int sc;
   public int min;
   public int max;
   public int geo_state;
   public int pre_state;
   public short[] my_baf;
   public short[] cur_obj;
   public boolean obj_chg;
   public short timesteps;
   public short teacher_state;
   private short my_param_val[];
   public double[] design_param_short;
}
