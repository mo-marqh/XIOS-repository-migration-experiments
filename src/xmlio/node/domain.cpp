#include "domain.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

#include "mpi_manager.hpp"

#include "tree_manager.hpp"

#include <algorithm>

namespace xmlioserver {
namespace tree {
   
   /// ////////////////////// Définitions ////////////////////// ///

   CDomain::CDomain(void)
      : CObjectTemplate<CDomain>(), CDomainAttributes()
      , isChecked(false), local_mask(new CArray<int, 2>(boost::extents[0][0])), relFiles()
      , ibegin_sub(), iend_sub(), jbegin_sub(), jend_sub()
      , ibegin_zoom_sub(), jbegin_zoom_sub(), ni_zoom_sub(), nj_zoom_sub()
      , lonvalue_sub(), latvalue_sub()
   { /* Ne rien faire de plus */ }

   CDomain::CDomain(const StdString & id)
      : CObjectTemplate<CDomain>(id), CDomainAttributes()
      , isChecked(false), local_mask(new CArray<int, 2>(boost::extents[0][0])), relFiles()
      , ibegin_sub(), iend_sub(), jbegin_sub(), jend_sub()
      , ibegin_zoom_sub(), jbegin_zoom_sub(),ni_zoom_sub(), nj_zoom_sub()
      , lonvalue_sub(), latvalue_sub()
   { /* Ne rien faire de plus */ }

   CDomain::~CDomain(void)
   { 
      this->local_mask.reset();
      for (StdSize i = 0; i < this->lonvalue_sub.size(); i++)
      {
         this->lonvalue_sub[i].reset();
         this->latvalue_sub[i].reset();
      }     
   }

   ///---------------------------------------------------------------

   const std::set<StdString> & CDomain::getRelFiles(void) const
   {
      return (this->relFiles);
   }

   //----------------------------------------------------------------
   
   bool CDomain::hasZoom(void) const
   {
      return ((this->zoom_ni.getValue() != this->ni_glo.getValue()) && 
              (this->zoom_nj.getValue() != this->nj_glo.getValue()));
   }
   
   //----------------------------------------------------------------
   
   bool CDomain::isEmpty(void) const
   {
      return ((this->zoom_ni_loc.getValue() == 0) || 
              (this->zoom_nj_loc.getValue() == 0));
   }

   //----------------------------------------------------------------

   bool CDomain::IsWritten(const StdString & filename) const
   {
      return (this->relFiles.find(filename) != this->relFiles.end());
   }

   //----------------------------------------------------------------

   void CDomain::addRelFile(const StdString & filename)
   {
      this->relFiles.insert(filename);
   }

   //----------------------------------------------------------------

   void CDomain::fromBinary(StdIStream & is)
   {
      SuperClass::fromBinary(is);
      
      if ( !this->ibegin.isEmpty()   &&
           !this->jbegin.isEmpty()   &&
           !this->iend.isEmpty()     &&
           !this->jend.isEmpty()     &&
           !this->latvalue.isEmpty() &&
           !this->lonvalue.isEmpty())
      {
      
         this->ibegin_sub.push_back(this->ibegin.getValue());
         this->jbegin_sub.push_back(this->jbegin.getValue());
         this->iend_sub.push_back(this->iend.getValue());
         this->jend_sub.push_back(this->jend.getValue()); 
         
         this->ibegin_zoom_sub.push_back(this->zoom_ibegin_loc.getValue());
         this->jbegin_zoom_sub.push_back(this->zoom_jbegin_loc.getValue());
         this->ni_zoom_sub.push_back(this->zoom_ni_loc.getValue());
         this->nj_zoom_sub.push_back(this->zoom_nj_loc.getValue());
      
         this->latvalue_sub.push_back(this->latvalue.getValue());
         this->lonvalue_sub.push_back(this->lonvalue.getValue());
      }
      
#define CLEAR_ATT(name_)\
      SuperClassAttribute::operator[](#name_)->clear()

         CLEAR_ATT(mask);
         CLEAR_ATT(data_n_index);
         CLEAR_ATT(data_i_index);
         CLEAR_ATT(data_j_index);
         
         CLEAR_ATT(data_ni);
         CLEAR_ATT(data_nj);
         CLEAR_ATT(data_ibegin);
         CLEAR_ATT(data_jbegin);
         
         CLEAR_ATT(ni);
         CLEAR_ATT(nj);
         
#undef CLEAR_ATT

      if ( !this->ibegin.isEmpty()   &&
           !this->jbegin.isEmpty()   &&
           !this->iend.isEmpty()     &&
           !this->jend.isEmpty()     &&
           !this->latvalue.isEmpty() &&
           !this->lonvalue.isEmpty())
      {

         this->ibegin.setValue(*std::min_element(this->ibegin_sub.begin(),this->ibegin_sub.end()));
         this->jbegin.setValue(*std::min_element(this->jbegin_sub.begin(),this->jbegin_sub.end()));
         this->iend.setValue(*std::max_element(this->iend_sub.begin(),this->iend_sub.end()));
         this->jend.setValue(*std::max_element(this->jend_sub.begin(),this->jend_sub.end()));
      }
   }

   //----------------------------------------------------------------

   StdString CDomain::GetName(void)   { return (StdString("domain")); }
   StdString CDomain::GetDefName(void){ return (CDomain::GetName()); }
   ENodeType CDomain::GetType(void)   { return (eDomain); }

   //----------------------------------------------------------------

   void CDomain::checkGlobalDomain(void)
   {
      if ((ni_glo.isEmpty() || ni_glo.getValue() <= 0 ) ||
          (nj_glo.isEmpty() || nj_glo.getValue() <= 0 ))
      {
         ERROR("CDomain::checkAttributes(void)",
               << "[ Id = " << this->getId() << " ] "
               << "Le domaine global est mal défini,"
               << " vérifiez les valeurs de \'ni_glo\' et \'nj_glo\' !") 
      }
   }

   //----------------------------------------------------------------

   void CDomain::checkLocalIDomain(void)
   {
      if (!ni.isEmpty() && !ibegin.isEmpty() && iend.isEmpty())
         iend.setValue(ibegin.getValue() + ni.getValue() - 1) ;

      else if (!ni.isEmpty() && !iend.isEmpty()   && ibegin.isEmpty())
         ibegin.setValue( - ni.getValue() + iend.getValue() + 1) ;

      else if (!ibegin.isEmpty() && !iend.isEmpty() && ni.isEmpty())
         ni.setValue(iend.getValue() - ibegin.getValue() + 1) ;

      else if (!ibegin.isEmpty() && !iend.isEmpty() &&
               !ni.isEmpty() && (iend.getValue() != ibegin.getValue() + ni.getValue() - 1))
      {
         ERROR("CDomain::checkAttributes(void)",
               << "Le domaine est mal défini,"
               << " iend est différent de (ibegin + ni - 1) !") ;
      }
      else
      {
         ERROR("CDomain::checkAttributes(void)",
               << "Le domaine est mal défini,"
               << " deux valeurs au moins parmis iend, ibegin, ni doivent être définies !") ;
      }


      if (ni.getValue() < 0 || ibegin.getValue() > iend.getValue() ||
          ibegin.getValue() < 1 || iend.getValue() > ni_glo.getValue())
         ERROR("CDomain::checkAttributes(void)",
               << "[ Id = " << this->getId() << " ] "
               << "Domaine local mal défini,"
               << " vérifiez les valeurs ni, ni_glo, ibegin, iend") ;

   }

   //----------------------------------------------------------------

   void CDomain::checkLocalJDomain(void)
   {
      if (!nj.isEmpty() && !jbegin.isEmpty() && jend.isEmpty())
         jend.setValue(jbegin.getValue() + nj.getValue() - 1) ;

      else if (!nj.isEmpty() && !jend.isEmpty() && jbegin.isEmpty())
         jbegin.setValue( - nj.getValue() + jend.getValue() + 1) ;

      else if (!jbegin.isEmpty() && !jend.isEmpty() && nj.isEmpty())
         nj.setValue(jend.getValue() - jbegin.getValue() + 1) ;

      else if (!jbegin.isEmpty() && !jend.isEmpty() && !nj.isEmpty() &&
               (jend.getValue() != jbegin.getValue() + nj.getValue() - 1))
      {
         ERROR("CDomain::checkAttributes(void)",
               << "Le domaine est mal défini,"
               << " iend est différent de (jbegin + nj - 1) !") ;
      }
      else
      {
         ERROR("CDomain::checkAttributes(void)",
               << "Le domaine est mal défini,"
               << " deux valeurs au moins parmis jend, jbegin, nj doivent être définies !") ;
      }

      if (nj.getValue() < 0 || jbegin.getValue() > jend.getValue() ||
          jbegin.getValue() < 1 || jend.getValue() > nj_glo.getValue())
         ERROR("CDomain::checkAttributes(void)",
               << "Domaine local mal défini,"
               << " vérifiez les valeurs nj, nj_glo, jbegin, jend") ;
   }

   //----------------------------------------------------------------

   void CDomain::checkMask(void)
   {
      using namespace std;
      
      int ibegin_mask = 0,
          jbegin_mask = 0,
          iend_mask = iend.getValue() - ibegin.getValue(),
          jend_mask = jend.getValue() - jbegin.getValue();
      
      if (!zoom_ibegin.isEmpty())
      {
         int zoom_iend = zoom_ibegin.getValue() + zoom_ni.getValue() - 1;
         int zoom_jend = zoom_jbegin.getValue() + zoom_nj.getValue() - 1;
         
         ibegin_mask = max (ibegin.getValue(), zoom_ibegin.getValue());
         jbegin_mask = max (jbegin.getValue(), zoom_jbegin.getValue());
         iend_mask   = min (iend.getValue(), zoom_iend);
         jend_mask   = min (jend.getValue(), zoom_jend);
                 
         ibegin_mask -= ibegin.getValue();
         jbegin_mask -= jbegin.getValue();
         iend_mask   -= ibegin.getValue();
         jend_mask   -= jbegin.getValue();
      }
      
      //~ std::cout << "-------------------" << std::endl
                //~ << "zoom : " << std::boolalpha << this->hasZoom() << std::endl
                //~ << "size : " << ni.getValue()  << " X " << nj.getValue()   << std::endl
                //~ << "it : " << ibegin.getValue() << ", " << iend.getValue() << std::endl
                //~ << "jt : " << jbegin.getValue() << ", " << jend.getValue() << std::endl
                //~ << "im : " << ibegin_mask << ", " << iend_mask << std::endl
                //~ << "jm : " << jbegin_mask << ", " << jend_mask << std::endl
                //~ << "-------------------" << std::endl;

      if (!mask.isEmpty())
      {
         ARRAY(bool, 2) mask_ = mask.getValue();
         unsigned int niu = ni.getValue(), nju = nj.getValue();
         if ((mask_->shape()[0] != niu) ||
             (mask_->shape()[1] != nju))
            ERROR("CDomain::checkAttributes(void)",
                  <<"Le masque n'a pas la même taille que le domaine local") ;
                  
         for (int i = 0; i < ni.getValue(); i++)
         {
            for (int j = 0; j < nj.getValue(); j++)
            {
               if (i < ibegin_mask && i > iend_mask &&
                   j < jbegin_mask && j > jend_mask )
                     (*mask_)[i][j] = false;
            }
         }
      }
      else // (!mask.hasValue())
      { // Si aucun masque n'est défini,
        // on en crée un nouveau qui valide l'intégralité du domaine.
         ARRAY_CREATE(__arr, bool, 2, [ni.getValue()][nj.getValue()]);
         for (int i = 0; i < ni.getValue(); i++)
         {
            for (int j = 0; j < nj.getValue(); j++)
            {
               if (i >= ibegin_mask && i <= iend_mask &&
                   j >= jbegin_mask && j <= jend_mask )
                     (*__arr)[i][j] = true;
               else  (*__arr)[i][j] = false;
            }
         }
               
         mask.setValue(__arr);
         __arr.reset();
      }
   }


   //----------------------------------------------------------------

   void CDomain::checkDomainData(void)
   {     
      if (!data_dim.isEmpty() &&
         !(data_dim.getValue() == 1 || data_dim.getValue() == 2))
      {
         ERROR("CDomain::checkAttributes(void)",
               << "Dimension des données non comptatible (doit être 1 ou 2) !") ;
      }
      else if (data_dim.isEmpty())
      {
         ERROR("CDomain::checkAttributes(void)",
               << "Dimension des données non définie !") ;
      }

      if (data_ibegin.isEmpty())
         data_ibegin.setValue(0) ;
      if (data_jbegin.isEmpty() && (data_dim.getValue() == 2))
         data_jbegin.setValue(0) ;

      if (!data_ni.isEmpty() && (data_ni.getValue() <= 0))
      {
         ERROR("CDomain::checkAttributes(void)",
               << "Dimension des données négative (data_ni).") ;
      }
      else if (data_ni.isEmpty())
      {
         data_ni.setValue((data_dim.getValue() == 1)
                           ? (ni.getValue() * nj.getValue())
                           : ni.getValue());
      }

      if (data_dim.getValue() == 2)
      {
         if (!data_nj.isEmpty() && (data_nj.getValue() <= 0) )
         {
            ERROR("CDomain::checkAttributes(void)",
                  << "Dimension des données négative (data_nj).") ;
         }
         else if (data_nj.isEmpty())
            data_nj.setValue(nj.getValue()) ;
      }

   }

   //----------------------------------------------------------------

   void CDomain::checkCompression(void)
   {
      if (!data_i_index.isEmpty())
      {
         int ssize = data_i_index.getValue()->size();
         if (!data_n_index.isEmpty() &&
            (data_n_index.getValue() != ssize))
         {
            ERROR("CDomain::checkAttributes(void)",
                  <<"Dimension data_i_index incompatible avec data_n_index.") ;
         }
         else if (data_n_index.isEmpty())
            data_n_index.setValue(ssize) ;

         if (data_dim.getValue() == 2)
         {
            if (!data_j_index.isEmpty() &&
               (data_j_index.getValue()->size() != data_i_index.getValue()->size()))
            {
               ERROR("CDomain::checkAttributes(void)",
                     <<"Dimension data_j_index incompatible avec data_i_index.") ;
            }
            else if (data_j_index.isEmpty())
            {
               ERROR("CDomain::checkAttributes(void)",
                     <<"La donnée data_j_index doit être renseignée !") ;
            }
         }
      }
      else
      {
         if (!data_n_index.isEmpty() ||
            ((data_dim.getValue() == 2) && (!data_j_index.isEmpty())))
            ERROR("CDomain::checkAttributes(void)", << "data_i_index non défini") ;
      }

      if (data_n_index.isEmpty())
      { // -> bloc re-vérifié OK
         if (data_dim.getValue() == 1)
         {
            const int dni = data_ni.getValue();
            ARRAY_CREATE(__arri, int, 1, [dni]);
            data_n_index.setValue(dni);
            for (int i = 0; i < dni; i++)
               (*__arri)[i] = i+1 ;
            data_i_index.setValue(__arri) ;
         }
         else   // (data_dim == 2)
         {
            const int dni = data_ni.getValue() * data_nj.getValue();
            
            ARRAY_CREATE(__arri, int, 1, [dni]);
            ARRAY_CREATE(__arrj, int, 1, [dni]);               
            data_n_index.setValue(dni);
            
            //for(int count = 0, i = 0; i  < data_ni.getValue(); i++)
            //for(int j = 0; j < data_nj.getValue(); j++, count++)
            
            for(int count = 0, j = 0; j  < data_nj.getValue(); j++)
            {
               for(int i = 0; i < data_ni.getValue(); i++, count++)
               {
                  (*__arri)[count] = i+1 ;
                  (*__arrj)[count] = j+1 ;
               }
            }
            data_i_index.setValue(__arri) ;
            data_j_index.setValue(__arrj) ;            
            __arri.reset();
            __arrj.reset();
         }
      }
   }

   //----------------------------------------------------------------
   
   void CDomain::completeLonLatClient(void)
   {
      ARRAY_CREATE(lonvalue_temp, double, 1, [0]);
      ARRAY_CREATE(latvalue_temp, double, 1, [0]);
      
      const int zoom_ibegin_client  = zoom_ibegin_loc.getValue(),
                zoom_jbegin_client  = zoom_jbegin_loc.getValue(),
                zoom_ni_client      = zoom_ni_loc.getValue(),
                zoom_nj_client      = zoom_nj_loc.getValue();
                
      ARRAY(double, 1) lonvalue_ = this->lonvalue.getValue(),
                       latvalue_ = this->latvalue.getValue();
                
      if (this->data_dim.getValue() == 2)
      {
         StdSize dm = zoom_ni_client * zoom_nj_client;

         lonvalue_temp->resize(boost::extents[dm]);
         latvalue_temp->resize(boost::extents[dm]);
         
         for (int i = 0; i < zoom_ni_client; i++)
         {
            for (int j = 0; j < zoom_nj_client; j++)
            {
               (*lonvalue_temp)[i + j * zoom_ni_client] =
               (*lonvalue_)[(i + zoom_ibegin_client -1)+(j + zoom_jbegin_client -1)*ni.getValue()];              
               (*latvalue_temp)[i + j * zoom_ni_client] =
               (*latvalue_)[(i + zoom_ibegin_client -1)+(j + zoom_jbegin_client -1)*ni.getValue()];
            }
         }
         this->lonvalue.setValue(lonvalue_temp);
         this->latvalue.setValue(latvalue_temp);
      }
      else
      {
         lonvalue_temp->resize(boost::extents[zoom_ni_client]);
         latvalue_temp->resize(boost::extents[zoom_nj_client]);
         
         for (int i = zoom_ibegin_client - 1; i < (zoom_ni_client - zoom_ibegin_client + 1); i++)
         {
            (*lonvalue_temp)[i] = (*lonvalue_)[i]; 
         }
         
         for (int j = zoom_ibegin_client - 1; j < (zoom_nj_client - zoom_jbegin_client + 1); j++)
         {
            (*latvalue_temp)[j] = (*latvalue_)[j];
         }
         
         this->lonvalue.setValue(lonvalue_temp);
         this->latvalue.setValue(latvalue_temp);
      }  
   }
 
   //----------------------------------------------------------------
      
   void CDomain::completeLonLatServer(void)
   {
      ARRAY_CREATE(lonvalue_temp, double, 1, [0]);
      ARRAY_CREATE(latvalue_temp, double, 1, [0]);
      
      const int ibegin_serv     = ibegin.getValue(),
                jbegin_serv     = jbegin.getValue(),
                zoom_ni_serv    = zoom_ni_loc.getValue(),
                zoom_nj_serv    = zoom_nj_loc.getValue(),
                ibegin_zoom_srv = zoom_ibegin_loc.getValue(),
                jbegin_zoom_srv = zoom_jbegin_loc.getValue();
                      
      /*std::cout << "Rang du serveur :" << comm::CMPIManager::GetCommRank()   << std::endl
                << "Begin serv : "     << ibegin_serv << ", " << jbegin_serv <<  std::endl
                << "End serv : "       << iend_serv   << ", " << jend_serv   <<  std::endl
                << "Zoom_loc begin : " << zoom_ibegin_loc << ", " << zoom_jbegin_loc <<  std::endl
                << "Zoom_loc size : "  << zoom_ni_loc << ", " << zoom_nj_loc <<  std::endl;*/
                       
      if (this->data_dim.getValue() == 2)
      {
         StdSize dm = zoom_ni_serv * zoom_nj_serv;      
         
         lonvalue_temp->resize(boost::extents[dm]);
         latvalue_temp->resize(boost::extents[dm]);
         
         for (StdSize k = 0; k < lonvalue_sub.size(); k++)
         {
            ARRAY(double, 1) lonvalue_loc = this->lonvalue_sub[k],
                             latvalue_loc = this->latvalue_sub[k];
            const int zoom_ibegin_cl = ibegin_zoom_sub[k], zoom_ni_cl = ni_zoom_sub[k],
                      zoom_jbegin_cl = jbegin_zoom_sub[k], zoom_nj_cl = nj_zoom_sub[k],
                      ibegin_cl = ibegin_sub[k] ,
                      jbegin_cl = jbegin_sub[k] ,
                      ni_cl = iend_sub[k] - ibegin_sub[k] + 1;
                      
            for (int i = 0; i < zoom_ni_cl; i++)
            {
               for (int j = 0; j < zoom_nj_cl; j++)
               {
                  int ii = i + (ibegin_cl-1) - (ibegin_serv - 1) + (zoom_ibegin_cl - 1) - (ibegin_zoom_srv - 1);
                  int jj = j + (jbegin_cl-1) - (jbegin_serv - 1) + (zoom_jbegin_cl - 1) - (jbegin_zoom_srv - 1);
                  (*lonvalue_temp)[ii + jj * zoom_ni_serv] =
                  (*lonvalue_loc)[i + j * zoom_ni_cl];
                  (*latvalue_temp)[ii + jj * zoom_ni_serv] = 
                  (*latvalue_loc)[i + j * zoom_ni_cl];
               }
            }
         }
         this->lonvalue.setValue(lonvalue_temp);
         this->latvalue.setValue(latvalue_temp);
      }
      else
      {
         lonvalue_temp->resize(boost::extents[zoom_ni_serv]);
         latvalue_temp->resize(boost::extents[zoom_nj_serv]);
         
         for (StdSize k = 0; k < lonvalue_sub.size(); k++)
         {
            ARRAY(double, 1) lonvalue_loc = this->lonvalue_sub[k],
                             latvalue_loc = this->latvalue_sub[k];
            const int zoom_ibegin_cl = ibegin_zoom_sub[k], zoom_ni_cl = ni_zoom_sub[k],
                      zoom_jbegin_cl = jbegin_zoom_sub[k], zoom_nj_cl = nj_zoom_sub[k];
                      
            for (int i = 0; i < zoom_ni_cl; i++)
               (*lonvalue_temp)[i /*- (ibegin_serv - 1)*/ + (zoom_ibegin_cl - 1) - (ibegin_zoom_srv - 1)] =
               (*lonvalue_loc)[i];
               
            for (int j = 0; j < zoom_nj_cl; j++)
               (*latvalue_temp)[j /*- (jbegin_serv - 1)*/ + (zoom_jbegin_cl - 1) - (jbegin_zoom_srv - 1)] =
               (*latvalue_loc)[j];
         }       
         this->lonvalue.setValue(lonvalue_temp);
         this->latvalue.setValue(latvalue_temp);
      }
   }

   //----------------------------------------------------------------

   void CDomain::checkZoom(void)
   {
      // Résolution et vérification des données globales de zoom.
      if (!this->zoom_ni.isEmpty() || !this->zoom_nj.isEmpty() ||
          !this->zoom_ibegin.isEmpty() || !this->zoom_jbegin.isEmpty())
      {
         if (this->zoom_ni.isEmpty()     && this->zoom_nj.isEmpty() &&
             this->zoom_ibegin.isEmpty() && this->zoom_jbegin.isEmpty())
         {
            ERROR("CDomain::checkZoom(void)",
                  <<"Les attributs définissant un zoom doivent tous être définis") ;
         }
         else
         {
            int zoom_iend = zoom_ibegin.getValue() + zoom_ni.getValue() - 1;
            int zoom_jend = zoom_jbegin.getValue() + zoom_nj.getValue() - 1;
                
            if (zoom_ibegin.getValue() < 1  || zoom_jbegin.getValue() < 1 ||
                zoom_iend > ni_glo.getValue() || zoom_jend > nj_glo.getValue())
               ERROR("CDomain::checkZoom(void)",
                     << "Zoom mal défini,"
                     << " vérifiez les valeurs zoom_ni, zoom_nj, zoom_ibegin, zoom_ibegin") ;
         }
      }
      else
      {
         this->zoom_ni.setValue(this->ni_glo.getValue()); 
         this->zoom_nj.setValue(this->nj_glo.getValue());
         this->zoom_ibegin.setValue(1);
         this->zoom_jbegin.setValue(1);
      }
      // Résolution des données locales de zoom.
      {
         int zoom_iend = zoom_ibegin.getValue() + zoom_ni.getValue() - 1;
         int zoom_jend = zoom_jbegin.getValue() + zoom_nj.getValue() - 1;
         
         if ((zoom_ibegin.getValue() > iend.getValue()) || 
             (zoom_iend < ibegin.getValue()))
         {
            zoom_ni_loc.setValue(0);
            zoom_ibegin_loc.setValue(zoom_ibegin.getValue());
         }
         else
         {
            int zoom_ibegin_loc_ = (zoom_ibegin.getValue() > ibegin.getValue()) 
                                 ? zoom_ibegin.getValue()
                                 : ibegin.getValue();
            int zoom_iend_loc_  = (zoom_iend < iend.getValue()) 
                                 ? zoom_iend
                                 : iend.getValue();
            int zoom_ni_loc_ = zoom_iend_loc_ - zoom_ibegin_loc_ + 1;
            
            zoom_ni_loc.setValue(zoom_ni_loc_);
            zoom_ibegin_loc.setValue(zoom_ibegin_loc_-ibegin.getValue()+1);
         }
         
         if ((zoom_jbegin.getValue() > jend.getValue()) || 
             (zoom_jend < jbegin.getValue()))
         {
            zoom_nj_loc.setValue(0);
            zoom_jbegin_loc.setValue(zoom_jbegin.getValue());
         }
         else
         {
            int zoom_jbegin_loc_ = (zoom_jbegin.getValue() > jbegin.getValue()) 
                                 ? zoom_jbegin.getValue()
                                 : jbegin.getValue();
            int zoom_jend_loc_  = (zoom_jend < jend.getValue()) 
                                 ? zoom_jend
                                 : jend.getValue();
            int zoom_nj_loc_ = zoom_jend_loc_ - zoom_jbegin_loc_ + 1;
            
            zoom_nj_loc.setValue(zoom_nj_loc_);
            zoom_jbegin_loc.setValue(zoom_jbegin_loc_-jbegin.getValue()+1);
         }
      }
   }

   //----------------------------------------------------------------

   void CDomain::checkAttributes(void)
   {
      if (this->isChecked) return;

      this->checkGlobalDomain();
      this->checkLocalIDomain();
      this->checkLocalJDomain();
      
      this->checkZoom();

      if (this->latvalue_sub.size() == 0)
      { // Côté client uniquement
         this->checkMask();
         this->checkDomainData();
         this->checkCompression();
         
         this->ibegin_sub.push_back(this->ibegin.getValue());
         this->jbegin_sub.push_back(this->jbegin.getValue());
         this->iend_sub.push_back(this->iend.getValue());
         this->jend_sub.push_back(this->jend.getValue()); 

         this->ibegin_zoom_sub.push_back(this->zoom_ibegin_loc.getValue());
         this->jbegin_zoom_sub.push_back(this->zoom_jbegin_loc.getValue());
         this->ni_zoom_sub.push_back(this->zoom_ni_loc.getValue());
         this->nj_zoom_sub.push_back(this->zoom_nj_loc.getValue());
      
         this->latvalue_sub.push_back(this->latvalue.getValue());
         this->lonvalue_sub.push_back(this->lonvalue.getValue());  


//         if (!this->isEmpty())
//         {
            this->completeLonLatClient();
//         }
      }
      else
      { // Côté serveur uniquement
//         if (!this->isEmpty())
            this->completeLonLatServer();
      }
      this->completeMask();

      this->isChecked = true;
   }
   
   //----------------------------------------------------------------
   
   void CDomain::completeMask(void)
   {
      this->local_mask->resize(boost::extents[zoom_ni_loc.getValue()][zoom_nj_loc.getValue()]);
   }

   //----------------------------------------------------------------

   ARRAY(int, 2) CDomain::getLocalMask(void) const
   {
      return (this->local_mask);
   }
   
   //----------------------------------------------------------------
   
   const std::vector<int> & CDomain::getIBeginSub(void) const
   {
      return (this->ibegin_sub);
   }
   
   //----------------------------------------------------------------
   
   const std::vector<int> & CDomain::getIBeginZoomSub(void) const
   {
      return (this->ibegin_zoom_sub);
   }

   const std::vector<int> & CDomain::getNiZoomSub(void) const
   {
      return (this->ni_zoom_sub);
   }
               
   //----------------------------------------------------------------
                     
   const std::vector<int> & CDomain::getIEndSub(void) const
   {
      return (this->iend_sub);
   }
   
   //----------------------------------------------------------------
   
   const std::vector<int> & CDomain::getJBeginSub(void) const
   {
      return (this->jbegin_sub);
   }
   
   //----------------------------------------------------------------
      
   const std::vector<int> & CDomain::getJBeginZoomSub(void) const
   {
      return (this->jbegin_zoom_sub);
   }

   const std::vector<int> & CDomain::getNjZoomSub(void) const
   {
      return (this->nj_zoom_sub);
   }
                  
   
   //----------------------------------------------------------------
   
   const std::vector<int> & CDomain::getJEndSub(void) const
   {
      return (this->jend_sub);
   }
   
   //----------------------------------------------------------------
   
   const std::vector<ARRAY(double, 1)> & CDomain::getLonValueSub(void) const
   {
      return (this->lonvalue_sub);
   }
   
   //----------------------------------------------------------------
   
   const std::vector<ARRAY(double, 1)> & CDomain::getLatValueSub(void) const
   {
      return (this->latvalue_sub);
   }   
   
   ///---------------------------------------------------------------

} // namespace tree
} // namespace xmlioserver
