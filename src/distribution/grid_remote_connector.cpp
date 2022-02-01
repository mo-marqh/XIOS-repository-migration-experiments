#include "grid_remote_connector.hpp"
#include "client_client_dht_template.hpp"
#include "leader_process.hpp"
#include "mpi.hpp"



namespace xios
{
  /** 
   * \brief class constructor. 
   * \param srcView List of sources views.
   * \param dstView List of remotes views.
   * \param localComm Local MPI communicator
   * \param remoteSize Size of the remote communicator
   */ 
  CGridRemoteConnector::CGridRemoteConnector(vector<shared_ptr<CLocalView>>& srcView, vector<shared_ptr<CDistributedView>>& dstView, MPI_Comm localComm, int remoteSize) 
                       : srcView_(srcView), dstView_(dstView), localComm_(localComm), remoteSize_(remoteSize) 
  {}

  /** 
   * \brief class constructor. 
   * \param srcView List of sources views.
   * \param dstView List of remotes views.
   * \param localComm Local MPI communicator
   * \param remoteSize Size of the remote communicator
   */ 
  CGridRemoteConnector::CGridRemoteConnector(vector<shared_ptr<CLocalView>>& srcView, vector< shared_ptr<CLocalView> >& dstView, MPI_Comm localComm, int remoteSize) 
                       : srcView_(srcView), localComm_(localComm), remoteSize_(remoteSize) 
  {
    for(auto& it : dstView) dstView_.push_back((shared_ptr<CDistributedView>) it) ; 
  }


  /**
   * \brief Compute if each view composing the source grid and the remote grid is distributed or not. 
   *         Result is stored on internal attributes \b isSrcViewDistributed_ and \b isDstViewDistributed_.
   * \detail To compute this, a hash is computed for each array on indices. The hash must permutable, i.e. 
   *         the order of the list of global indices doesn't influence the value of the hash. So simply a sum of 
   *         hash of each indices is used for the whole array. After, the computed hash are compared with each other
   *         ranks of \b localComm_ MPI communicator using an MPI_ALLReduce. If, for each ranks, the hash is the same
   *         then the view is not distributed
   */
  void CGridRemoteConnector::computeViewDistribution(void)
  {
    HashXIOS<size_t> hashGlobalIndex; // hash function-object

    int nDst = dstView_.size() ;
    vector<size_t> hashRank(remoteSize_) ;
    isDstViewDistributed_.resize(nDst) ;

    for(int i=0; i<nDst; i++)
    {
      map<int,CArray<size_t,1>> globalIndexView ;
      dstView_[i]->getGlobalIndexView(globalIndexView) ;
      hashRank.assign(remoteSize_,0) ; // everybody ranks to 0 except rank of the remote view I have 
                                       // that would be assign to my local hash 
      for(auto& it : globalIndexView)
      {
        int rank=it.first ;
        CArray<size_t,1>& globalIndex = it.second ;
        size_t globalIndexSize = globalIndex.numElements();
        size_t hashValue=0 ;
        for(size_t ind=0;ind<globalIndexSize;ind++) hashValue += hashGlobalIndex(globalIndex(ind)) ;
        hashRank[rank] += hashValue ;
      }
      // sum all the hash for every process of the local comm. The reduce is on the size of remote view (remoteSize_)
      // after that for each rank of the remote view, we get the hash
      MPI_Allreduce(MPI_IN_PLACE, hashRank.data(), remoteSize_, MPI_SIZE_T, MPI_SUM, localComm_) ;
      size_t value = hashRank[0] ;
      isDstViewDistributed_[i]=false ;
      for(int j=0 ; j<remoteSize_ ; j++) 
        if (value != hashRank[j]) 
        { 
          isDstViewDistributed_[i]=true ;
          break ;
        }
    }

    int nSrc = srcView_.size() ;
    int commSize,commRank ;
    MPI_Comm_size(localComm_,&commSize) ;
    MPI_Comm_rank(localComm_,&commRank) ;
    hashRank.resize(commSize,0) ;
    isSrcViewDistributed_.resize(nSrc) ;

    for(int i=0; i<nSrc; i++)
    {
      CArray<size_t,1> globalIndex ;
      srcView_[i]->getGlobalIndexView(globalIndex) ;
      hashRank.assign(commSize,0) ; // 0 for everybody except my rank
      size_t globalIndexSize = globalIndex.numElements() ;
      size_t hashValue=0 ;
      for(size_t ind=0;ind<globalIndexSize;ind++) hashValue += hashGlobalIndex(globalIndex(ind)) ;
        hashRank[commRank] += hashValue ;
    
      // Same method than for remote view 
      MPI_Allreduce(MPI_IN_PLACE, hashRank.data(), commSize, MPI_SIZE_T, MPI_SUM, localComm_) ;
      size_t value = hashRank[0] ;
      isSrcViewDistributed_[i]=false ;
      for(int j=0 ; j<commSize ; j++) 
        if (value != hashRank[j]) 
        { 
          isSrcViewDistributed_[i]=true ;
          break ;
        }
    }

  }

/**
  * \brief Compute the connector, i.e. compute the \b elements_ attribute. 
  * \detail Depending of the distributions of the view computed in the computeViewDistribution() call, the connector is computed in computeConnectorMethods(), and to achieve better optimisation
  *         some redondant ranks can be removed from the elements_ map.
  */
  void CGridRemoteConnector::computeConnector(bool eliminateRedundant)
  {
    if (eliminateRedundant)
    {
      computeViewDistribution() ;
      computeConnectorMethods() ;
      computeRedondantRanks() ; 
      for(auto& rank : rankToRemove_)
        for(auto& element : elements_) element.erase(rank) ;
    }
    else
    {
       computeViewDistribution() ;
       computeConnectorRedundant() ;
    }
  }

/**
  * \brief Compute the connector, i.e. compute the \b elements_ attribute. 
  * \detail In this routine we don't eliminate redundant cells as it it performed in 
  *         computeConnectorMethods. It can be usefull to perform reduce operation over processes.
            In future, some optimisation could be done considering full redondance of the 
            source view or the destination view.
  */
  void CGridRemoteConnector::computeConnectorRedundant(void)
  {
    vector<shared_ptr<CLocalView>> srcView ;
    vector<shared_ptr<CDistributedView>> dstView ;
    vector<int> indElements ;
    elements_.resize(srcView_.size()) ;
    
    bool srcViewsNonDistributed=true ; // not usefull now but later for optimization
    for(int i=0;i<srcView_.size();i++) srcViewsNonDistributed = srcViewsNonDistributed && !isSrcViewDistributed_[i]  ;
    
    bool dstViewsNonDistributed=true ;  // not usefull now but later for optimization
    for(int i=0;i<dstView_.size();i++) dstViewsNonDistributed = dstViewsNonDistributed && !isDstViewDistributed_[i] ;
    
    for(int i=0;i<srcView_.size();i++) 
    {
      srcView.push_back(srcView_[i]) ;
      dstView.push_back(dstView_[i]) ;
      indElements.push_back(i) ;
    }

    computeGenericMethod(srcView, dstView, indElements) ;
    
    map<int,bool> ranks ;  
    for(auto& it : elements_[indElements[0]]) 
    {
      if (it.second.numElements()==0) ranks[it.first] = false ;
      else  ranks[it.first] = true ;
    }
   
  }


/**
  * \brief Compute the connector, i.e. compute the \b elements_ attribute. 
  * \detail In order to achive better optimisation,
  *         we distingute the case when the grid is not distributed on source grid (\bcomputeSrcNonDistributed), 
  *         or the remote grid (\b computeDstNonDistributed), or the both (\b computeSrcDstNonDistributed). 
  *         Otherwise the generic method is called computeGenericMethod. Note that in the case, if one element view
  *         is not distributed on the source and on the remote grid, then we can used the tensorial product
  *         property to computing it independently using \b computeSrcDstNonDistributed method.
  *         After that, we call the \b removeRedondantRanks method to supress blocks of data that can be sent 
  *         redondantly the the remote servers
  */
  void CGridRemoteConnector::computeConnectorMethods(void)
  {
    vector<shared_ptr<CLocalView>> srcView ;
    vector<shared_ptr<CDistributedView>> dstView ;
    vector<int> indElements ;
    elements_.resize(srcView_.size()) ;
    
    bool srcViewsNonDistributed=true ;
    for(int i=0;i<srcView_.size();i++) srcViewsNonDistributed = srcViewsNonDistributed && !isSrcViewDistributed_[i]  ;
    
    bool dstViewsNonDistributed=true ;
    for(int i=0;i<dstView_.size();i++) dstViewsNonDistributed = dstViewsNonDistributed && !isDstViewDistributed_[i] ;
    
    if (srcViewsNonDistributed) 
    {
      int commRank, commSize ;
      MPI_Comm_rank(localComm_,&commRank) ;
      MPI_Comm_size(localComm_,&commSize) ;
      list<int> remoteRanks;
      list<int> notUsed ;
      map<int,bool> ranks ;  
      computeLeaderProcess(commRank, commSize, remoteSize_, remoteRanks, notUsed) ;
      for(int rank : remoteRanks) ranks[rank]=true ;
      
      for(int i=0; i<srcView_.size(); i++)  
      {
        if (isDstViewDistributed_[i]) computeSrcNonDistributed(i) ;
        else computeSrcDstNonDistributed(i, ranks) ;
      }
    } 
    else if (dstViewsNonDistributed)
    {
      map<int,bool> ranks ;
      for(int i=0;i<remoteSize_;i++) ranks[i]=true ;
      for(int i=0; i<srcView_.size(); i++)  
      {
        if (isSrcViewDistributed_[i]) computeDstNonDistributed(i,ranks) ;
        else computeSrcDstNonDistributed(i,ranks) ;
      }
    } 
    else
    {
      for(int i=0;i<srcView_.size();i++) 
        if (isSrcViewDistributed_[i] || isDstViewDistributed_[i])
        {
          srcView.push_back(srcView_[i]) ;
          dstView.push_back(dstView_[i]) ;
          indElements.push_back(i) ;
        }

      computeGenericMethod(srcView, dstView, indElements) ;
    
      map<int,bool> ranks ;  
      for(auto& it : elements_[indElements[0]]) 
      {
        if (it.second.numElements()==0) ranks[it.first] = false ;
        else  ranks[it.first] = true ;
      }
    
      for(int i=0;i<srcView_.size();i++) 
        if (!isSrcViewDistributed_[i] && !isDstViewDistributed_[i]) computeSrcDstNonDistributed(i, ranks) ;
    }

  }

  
/**
  * \brief Compute the connector for the element \b i when the source view is not distributed. 
  *        After the call element_[i] is defined.
  *  \param i Indice of the element composing the source grid. 
  */

  void CGridRemoteConnector::computeSrcNonDistributed(int i)
  {
    auto& element = elements_[i] ;
    map<int,CArray<size_t,1>> globalIndexView ;
    dstView_[i]->getGlobalIndexView(globalIndexView) ;
    
    CClientClientDHTTemplate<int>::Index2InfoTypeMap dataInfo;
    
    for(auto& it : globalIndexView)
    {
      auto& globalIndex=it.second ;
      for(size_t ind : globalIndex) dataInfo[ind]=it.first ;
    }
    
    // First we feed the distributed hash map  with key (remote global index) 
    // associated with the value of the remote rank
    CClientClientDHTTemplate<int> DHT(dataInfo, localComm_) ;
    // after we feed the DHT with the local global indices of the source view

    int commRank, commSize ;
    MPI_Comm_rank(localComm_,&commRank) ;
    MPI_Comm_size(localComm_,&commSize) ;
    CArray<size_t,1> srcIndex ;
    // like the source view is not distributed, then only the rank 0 need to feed the DHT
    if (commRank==0) srcView_[i]->getGlobalIndexView(srcIndex) ;
    
    // compute the mapping
    DHT.computeIndexInfoMapping(srcIndex) ;
    auto& returnInfo = DHT.getInfoIndexMap() ;
    
    // returnInfo contains now the map for each global indices to send to a list of remote rank
    // only for the rank=0 because it is the one to feed the DHT
    // so it need to send the list to each server leader i.e. the local process that handle specifically one or more 
    // servers
    
    // rankIndGlo : rankIndGlo[rank][indGlo] : list of indice to send the the remote server of rank "rank"
    vector<vector<size_t>> rankIndGlo(remoteSize_) ;
    if (commRank==0) 
      for(auto& it1 : returnInfo)
        for(auto& it2 : it1.second) rankIndGlo[it2].push_back(it1.first) ;
    
    
    vector<MPI_Request> requests ;
    
    if (commRank==0)
    {
      requests.resize(remoteSize_) ;
      for(int i=0 ; i<remoteSize_;i++) 
      {
        // ok send only the global indices for a server to the "server leader"
        int rank = getLeaderRank(commSize, remoteSize_, i) ;
        MPI_Isend(rankIndGlo[i].data(), rankIndGlo[i].size(), MPI_SIZE_T, rank, i ,localComm_, &requests[i]) ;
      }
    }  
   
    list<int> remoteRanks;
    list<int> notUsed ;
    // I am a server leader of which remote ranks ?
    computeLeaderProcess(commRank, commSize, remoteSize_, remoteRanks, notUsed) ;

    for(auto remoteRank : remoteRanks)
    {
      MPI_Status status ;
      int size ;
      MPI_Probe(0,remoteRank,localComm_, &status);
      MPI_Get_count(&status, MPI_SIZE_T, &size) ;
      elements_[i][remoteRank].resize(size) ;
      // for each remote ranks receive the global indices from proc 0
      MPI_Recv(elements_[i][remoteRank].dataFirst(),size, MPI_SIZE_T,0,remoteRank, localComm_,&status) ;
    }
      
    if (commRank==0)
    {
      vector<MPI_Status> status(remoteSize_) ;
      // asynchronous for sender, wait for completion
      MPI_Waitall(remoteSize_, requests.data(), status.data()) ;
    }
  }

  /**
   * \brief Compute the remote connector for the element \b i when the remote view is not distributed. 
   *        After the call,  element_[i] is defined.
   * \param i Indice of the element composing the remote grid. 
   * \param ranks The list of rank for which the local proc is in charge to compute the connector 
   *              (if leader server for exemple). if ranks[rank] == false the corresponding elements_ 
   *              is set to void array (no data to sent) just in order to notify corresponding remote server
   *              that the call is collective with each other one  
   */
  void CGridRemoteConnector::computeDstNonDistributed(int i, map<int,bool>& ranks)
  {
    auto& element = elements_[i] ;
    map<int,CArray<size_t,1>> globalIndexView ;
    dstView_[i]->getGlobalIndexView(globalIndexView) ;
    
    
    CClientClientDHTTemplate<int>::Index2InfoTypeMap dataInfo;
 
    // First we feed the distributed hash map  with key (remote global index) 
    // associated with the value of the remote rank
    for(auto& it : globalIndexView)
      if (it.first==0) // since the remote view is not distributed, insert only the remote rank 0
      {
        auto& globalIndex=it.second ;
        for(size_t ind : globalIndex) dataInfo[ind]=0 ; // associated the the rank 0
      }
    
    CClientClientDHTTemplate<int> DHT(dataInfo, localComm_) ;
    // after we feed the DHT with the local global indices of the source view

    CArray<size_t,1> srcIndex ;
    srcView_[i]->getGlobalIndexView(srcIndex) ;
    DHT.computeIndexInfoMapping(srcIndex) ;
    auto& returnInfo = DHT.getInfoIndexMap() ;
    
    // returnInfo contains now the map for each global indices to send to a list of remote rank
    // now construct the element_ list of global indices for each rank in my list except if the erray must be empty
    for (auto& rank : ranks)
    {
      if (rank.second) // non empty array => for rank that have not any data to be received
      {
        int size=0 ;
        for(auto& it : returnInfo) if (!it.second.empty()) size++ ;
        auto& array = element[rank.first] ;
       array.resize(size) ;
       size=0 ;
       for(auto& it : returnInfo) 
         if (!it.second.empty()) 
         {
           array(size)=it.first ;
           size++ ;
         }
      }
      else element[rank.first] = CArray<size_t,1>(0) ;  // empty array => for rank that have not any data to be received
    }
  }

 /**
  * \brief Compute the remote connector for the element \b i when the source and the remote view are not distributed. 
  *        After the call, element_[i] is defined.
  * \param i Indice of the element composing the remote grid. 
  * \param ranks The list of rank for which the local proc is in charge to compute the connector 
  *              (if leader server for exemple). if ranks[rank] == false the corresponding elements_ 
  *              is set to void array (no data to sent) just in order to notify corresponding remote server
  *              that the call is collective with each other one  
  */

  void CGridRemoteConnector::computeSrcDstNonDistributed(int i, map<int,bool>& ranks)
  {
    auto& element = elements_[i] ;
    map<int,CArray<size_t,1>> globalIndexView ;
    dstView_[i]->getGlobalIndexView(globalIndexView) ;
    
    
    CClientClientDHTTemplate<int>::Index2InfoTypeMap dataInfo;
    // First we feed the distributed hash map  with key (remote global index) 
    // associated with the value of the remote rank

    for(auto& it : globalIndexView)
      if (it.first==0) // insert only the remote rank 0 since the remote view is not distributed
      {
        auto& globalIndex=it.second ;
        for(size_t ind : globalIndex) dataInfo[ind]=0 ; // associated the the rank 0
      }
    
    CClientClientDHTTemplate<int> DHT(dataInfo, localComm_) ;
    // after we feed the DHT with the local global indices of the source view

    int commRank, commSize ;
    MPI_Comm_rank(localComm_,&commRank) ;
    MPI_Comm_size(localComm_,&commSize) ;
    CArray<size_t,1> srcIndex ;
  
    // like the source view is not distributed, then only the rank 0 need to feed the DHT
    if (commRank==0) srcView_[i]->getGlobalIndexView(srcIndex) ;
    DHT.computeIndexInfoMapping(srcIndex) ;
    auto& returnInfo = DHT.getInfoIndexMap() ;
    
    vector<size_t> indGlo ;
    if (commRank==0) 
      for(auto& it1 : returnInfo) 
        for(auto& it2 : it1.second) indGlo.push_back(it1.first) ;

    // now local rank 0 know which indices to seed to remote rank 0, but all the server
    // must receive the same information. So only the leader rank will sent this.
    // So local rank 0 must broadcast the information to all leader.
    // for this we create a new communicator composed of local process that must send data
    // to a remote rank, data are broadcasted, and element_[i] is construction for each remote 
    // rank in charge
    int color=0 ;
    if (ranks.empty()) color=0 ;
    else color=1 ;
    if (commRank==0) color=1 ;
    MPI_Comm newComm ;
    MPI_Comm_split(localComm_, color, commRank, &newComm) ;
    if (color==1)
    {
      // ok, I am part of the process that must send something to one or more remote server
      // so I get the list of global indices from rank 0
      int dataSize ;
      if (commRank==0) dataSize=indGlo.size() ;
      MPI_Bcast(&dataSize,1,MPI_INT, 0, newComm) ;
      indGlo.resize(dataSize) ;
      MPI_Bcast(indGlo.data(),dataSize,MPI_SIZE_T,0,newComm) ;
    }
    MPI_Comm_free(&newComm) ;

    // construct element_[i] from indGlo
    for(auto& rank : ranks)
    {
      if (rank.second)
      {
        int dataSize=indGlo.size();
        auto& element = elements_[i][rank.first] ;
        element.resize(dataSize) ;
        for(int i=0;i<dataSize; i++) element(i)=indGlo[i] ;
      }
      else element[rank.first] = CArray<size_t,1>(0) ;
    }    

  }


 /**
  * \brief Generic method the compute the grid remote connector. Only distributed elements are specifed in the source view and remote view. 
  *        Connector for non distributed elements are computed separatly to improve performance and memory consumption. After the call,
  *        \b elements_  is defined.
  *  \param srcView List of the source views composing the grid, without non distributed views
  *  \param dstView List of the remote views composing the grid, without non distributed views
  *  \param indElements Index of the view making the correspondance between all views and views distributed (that are in input)
  */
  void CGridRemoteConnector::computeGenericMethod(vector<shared_ptr<CLocalView>>& srcView, vector<shared_ptr<CDistributedView>>& dstView, vector<int>& indElements)
  {
    // generic method, every element can be distributed
    int nDst = dstView.size() ;
    vector<size_t> dstSliceSize(nDst) ;
    dstSliceSize[0] = 1 ;  
    for(int i=1; i<nDst; i++)  dstSliceSize[i] = dstView[i-1]->getGlobalSize()*dstSliceSize[i-1] ;
  
    CClientClientDHTTemplate<int>::Index2VectorInfoTypeMap dataInfo ;
    CClientClientDHTTemplate<size_t>::Index2VectorInfoTypeMap info ; // info map

    // first, we need to feed the DHT with the global index of the remote server
    // for that :
    // First the first element insert the in a DHT with key as the rank and value the list of global index associated
    // Then get the previously stored index associate with the remote rank I am in charge and reinsert the global index
    // corresponding to the position of the element in the remote view suing tensorial product
    // finaly we get only the list of remote global index I am in charge for the whole remote grid   

    for(int pos=0; pos<nDst; pos++)
    {
      size_t sliceSize=dstSliceSize[pos] ;
      map<int,CArray<size_t,1>> globalIndexView ;
      dstView[pos]->getGlobalIndexView(globalIndexView) ;
      
      CClientClientDHTTemplate<size_t>::Index2VectorInfoTypeMap lastInfo(info) ;

      if (pos>0)
      {
        CArray<size_t,1> ranks(globalIndexView.size()) ;
        auto it=globalIndexView.begin() ;
        for(int i=0 ; i<ranks.numElements();i++,it++) ranks(i)=it->first ;
        CClientClientDHTTemplate<size_t> dataRanks(info, localComm_) ;
        dataRanks.computeIndexInfoMapping(ranks) ;
        lastInfo = dataRanks.getInfoIndexMap() ;
      }
      
      info.clear() ;
      for(auto& it : globalIndexView)
      {
        int rank = it.first ;
        auto& globalIndex = it.second ;
        auto& inf = info[rank] ;
        if (pos==0) for(int i=0;i<globalIndex.numElements();i++) inf.push_back(globalIndex(i)) ;
        else
        {
          auto& lastGlobalIndex = lastInfo[rank] ;
          for(size_t lastGlobalInd : lastGlobalIndex)
          {
            for(int i=0;i<globalIndex.numElements();i++) inf.push_back(globalIndex(i)*sliceSize+lastGlobalInd) ;
          }
        } 
      }

      if (pos==nDst-1)
      {
         for(auto& it : info)
         {
           int rank=it.first ;
           auto& globalIndex = it.second ;
           for(auto globalInd : globalIndex) dataInfo[globalInd].push_back(rank) ;
         }
      } 
    }

    // we feed the DHT with the remote global index
    CClientClientDHTTemplate<int> dataRanks(dataInfo, localComm_) ;

    // generate list of global index for src view
    int nSrc = srcView.size() ;
    vector<size_t> srcSliceSize(nSrc) ;
   
    srcSliceSize[0] = 1 ;  
    for(int i=1; i<nSrc; i++)  srcSliceSize[i] = srcView[i-1]->getGlobalSize()*srcSliceSize[i-1] ;

    vector<size_t> srcGlobalIndex ;
    size_t sliceIndex=0 ;
    srcView[nSrc-1]->getGlobalIndex(srcGlobalIndex, sliceIndex, srcSliceSize.data(), srcView.data(), nSrc-1) ;
    // now we have the global index of the source grid in srcGlobalIndex 
    // we feed the DHT with the src global index (if we have)
    if (srcGlobalIndex.size()>0)
    {
      CArray<size_t,1> srcGlobalIndexArray(srcGlobalIndex.data(), shape(srcGlobalIndex.size()),neverDeleteData) ;
      dataRanks.computeIndexInfoMapping(srcGlobalIndexArray) ;
    }
    else
    {
      CArray<size_t,1> srcGlobalIndexArray ;
      dataRanks.computeIndexInfoMapping(srcGlobalIndexArray) ;
    }
    const auto& returnInfo = dataRanks.getInfoIndexMap() ;
    // returnInfo contains now the map for each global indices to send to a list of remote rank
    // but we want to use the tensorial product property to get the same information using only global
    // index of element view. So the idea is to reverse the information : for a global index of the grid 
    // to send to the remote server, what is the global index of each element composing the grid ?

    vector<map<int, set<size_t>>> elements(nSrc) ; // internal representation of elements composing the grid

    for(auto& indRanks : returnInfo)
    {
      size_t gridIndexGlo=indRanks.first ;
      auto& ranks = indRanks.second ;
      for(int i=nSrc-1; i>=0; i--)
      {
        auto& element = elements[i] ;
        size_t localIndGlo = gridIndexGlo / srcSliceSize[i] ;
        gridIndexGlo = gridIndexGlo % srcSliceSize[i] ;
        for(int rank : ranks) element[rank].insert(localIndGlo) ;
      }
    }

//    elements_.resize(nSrc) ;
    for(int i=0 ; i<nSrc; i++)
    {
      auto& element=elements[i] ;
      for(auto& rankInd : element)
      {
        int rank=rankInd.first ;
        set<size_t>& indGlo = rankInd.second ;
        CArray<size_t,1>& indGloArray = elements_[indElements[i]][rank] ;
        indGloArray.resize(indGlo.size()) ;
        int j=0 ;
        for (auto index : indGlo) { indGloArray(j) = index ; j++; }
      }
    }
    
    // So what about when there is some server that have no data to receive
    // they must be inform they receive an event with no data.
    // So find remote servers with no data, and one client will take in charge 
    // that it receive global index with no data (0-size) 
    vector<int> ranks(remoteSize_,0) ;
    for(auto& it : elements_[indElements[0]]) ranks[it.first] = 1 ;
    MPI_Allreduce(MPI_IN_PLACE, ranks.data(), remoteSize_, MPI_INT, MPI_SUM, localComm_) ;
    int commRank, commSize ;
    MPI_Comm_rank(localComm_,&commRank) ;
    MPI_Comm_size(localComm_,&commSize) ;
    int pos=0 ;
    for(int i=0; i<remoteSize_ ; i++)
      if (ranks[i]==0)
      {
        if (pos%commSize==commRank) 
          for(int j=0 ; j<nSrc; j++) elements_[indElements[j]][i] = CArray<size_t,1>(0) ;
        pos++ ;
      }
  }

 /**
  * \brief Once the connector is computed (compute \b elements_), redondant data can be avoid to be sent to the server. 
  *        This call compute the redondant rank and store them in \b rankToRemove_ attribute.
  *        The goal of this method is to make a hash of each block of indice that determine wich data to send to a 
  *        of a specific server rank using a hash method. So data to send to a rank is associated to a hash.
  *        After we compare hash between local rank and remove redondant data corresponding to the same hash.
  */
  void CGridRemoteConnector::computeRedondantRanks(void)
  {
    int commRank ;
    MPI_Comm_rank(localComm_,&commRank) ;

    set<int> ranks;
    for(auto& element : elements_) 
      for(auto& it : element) ranks.insert(it.first) ;

    for(auto& element : elements_)
      for(auto& it : element) 
        if (ranks.count(it.first)==0) ERROR("void CGridRemoteConnector::removeRedondantRanks(void)",<<"number of ranks in elements is not coherent between each element") ;
    
    HashXIOS<size_t> hashGlobalIndex;
    
    map<int,size_t> hashRanks ;
    for(auto& element : elements_) 
      for(auto& it : element)
      {
        auto& globalIndex=it.second ;
        int rank=it.first ;
        size_t hash ;
        hash=0 ;
        for(int i=0; i<globalIndex.numElements(); i++) hash+=hashGlobalIndex(globalIndex(i)) ;
        if (hashRanks.count(rank)==0) hashRanks[rank]=hash ;
        else hashRanks[rank]=hashGlobalIndex.hashCombine(hashRanks[rank],hash) ;
      }
    // a hash is now computed for data block I will sent to the server.

    CClientClientDHTTemplate<int>::Index2InfoTypeMap info ;

    map<size_t,int> hashRank ;
    HashXIOS<int> hashGlobalIndexRank;
    for(auto& it : hashRanks) 
    {
      it.second = hashGlobalIndexRank.hashCombine(it.first,it.second) ; 
      info[it.second]=commRank ;
      hashRank[it.second]=it.first ;
    }

    // we feed a DHT map with key : hash, value : myrank
    CClientClientDHTTemplate<int> dataHash(info, localComm_) ;
    CArray<size_t,1> hashList(hashRank.size()) ;
    
    int i=0 ;
    for(auto& it : hashRank) { hashList(i)=it.first ; i++; }

    // now who are the ranks that have the same hash : feed the DHT with my list of hash
    dataHash.computeIndexInfoMapping(hashList) ;
    auto& hashRankList = dataHash.getInfoIndexMap() ;
    

    for(auto& it : hashRankList)
    {
      size_t hash = it.first ;
      auto& ranks = it.second ;
      
      bool first=true ;
      // only the process with the lowest rank get in charge of sendinf data to remote server
      for(int rank : ranks) if (commRank>rank) first=false ;
      if (!first) rankToRemove_.insert(hashRank[hash]) ;
    }
  }
  
}