%
%
%  Find Spots Close To Surface for Imaris 9.0.0
%
%  Copyright Bitplane AG 2017
%
%
%  Installation:
%
%  - Copy this file into the XTensions folder in the Imaris installation directory
%  - You will find this function in the surfaces and spots custom tools tab
%
%    <CustomTools>
%      <SurpassTab>
%        <SurpassComponent name="bpSpots">
%          <Item name="Find Spots Close To Surface AS" icon="Matlab" tooltip="Find spots close to surface.">
%            <Command>MatlabXT::XTSpotsCloseToSurfaceAS(%i)</Command>
%          </Item>
%        </SurpassComponent>
%        <SurpassComponent name="bpSurfaces">
%          <Item name="Find Spots Close To Surface AS" icon="Matlab" tooltip="Find spots close to surface.">
%            <Command>MatlabXT::XTSpotsCloseToSurfaceAS(%i)</Command>
%          </Item>
%        </SurpassComponent>
%      </SurpassTab>
%    </CustomTools>
% 
%
%  Description:
%   
%   Find the spots that are closer to a surface than a threshold, and 
%       create two new spots objects: One collects the spots close to the
%       surface, the other the farer. 
%   If multiple spots or surfaces are selected, each pair [spots, surface] 
%       is analyzed: The number of spots objects created is 2 times 
%       'number of spots' times 'number of surfaces'. 
%   ***MODIFICATIONS FROM ORIGINAL
%   This version omits the creation of Far Spots component so there are
%   fewer objects to export and sort.
%
%

function XTSpotsCloseToSurfaceAS(aImarisApplicationID)

% connect to Imaris interface
if ~isa(aImarisApplicationID, 'Imaris.IApplicationPrxHelper')
    javaaddpath ImarisLib.jar
    vImarisLib = ImarisLib;
    if ischar(aImarisApplicationID)
        aImarisApplicationID = round(str2double(aImarisApplicationID));
    end
    vImarisApplication = vImarisLib.GetApplication(aImarisApplicationID);
else
    vImarisApplication = aImarisApplicationID;
end

% the user has to create a scene with some spots and surface
vSurpassScene = vImarisApplication.GetSurpassScene;
if isequal(vSurpassScene, [])
    msgbox('Please create some Spots and Surface in the Surpass scene!')
    return
end

% get the spots and the surface object
vSpots = vImarisApplication.GetFactory.ToSpots(vImarisApplication.GetSurpassSelection);
vSurfaces = vImarisApplication.GetFactory.ToSurfaces(vImarisApplication.GetSurpassSelection);

vSpotsSelected = ~isequal(vSpots, []);
vSurfaceSelected = ~isequal(vSurfaces, []);
if vSpotsSelected
    vParent = vSpots.GetParent;
elseif vSurfaceSelected
    vParent = vSurfaces.GetParent;
else
    vParent = vSurpassScene;
end

% get the spots and surfaces
vSpotsSelection = 1;
vSurfaceSelection = 1;
vNumberOfSpots = 0;
vNumberOfSurfaces = 0;
vSpotsList = [];
vSurfacesList = [];
vSpotsName = {};
vSurfacesName = {};
for vIndex = 1:vParent.GetNumberOfChildren
  vItem = vParent.GetChild(vIndex-1);
  if vImarisApplication.GetFactory.IsSpots(vItem)
    vNumberOfSpots = vNumberOfSpots + 1;
    vSpotsList(vNumberOfSpots) = vIndex;
    vSpotsName{vNumberOfSpots} = char(vItem.GetName);
        
    if vSpotsSelected && isequal(vItem.GetName, vSpots.GetName)
      vSpotsSelection = vNumberOfSpots; 
    end
  elseif vImarisApplication.GetFactory.IsSurfaces(vItem)
    vNumberOfSurfaces = vNumberOfSurfaces + 1;
    vSurfacesList(vNumberOfSurfaces) = vIndex;
    vSurfacesName{vNumberOfSurfaces} = char(vItem.GetName);
        
    if vSurfaceSelected && isequal(vItem.GetName, vSurfaces.GetName)
      vSurfaceSelection = vNumberOfSurfaces;
    end
  end
end

if min(vNumberOfSpots,vNumberOfSurfaces) == 0
  msgbox('Please create some spots AND a surface object!')
  return
end

if vNumberOfSpots>1
  [vSpotsSelection,vOk] = listdlg('ListString',vSpotsName, ...
      'InitialValue', vSpotsSelection, 'SelectionMode','multiple', ...
      'ListSize',[300 300], 'Name','Find Spots Close To Surface', ...
      'PromptString',{'Please select the spots:'});
  if vOk<1, return, end
end
if vNumberOfSurfaces>1
  [vSurfaceSelection,vOk] = listdlg('ListString',vSurfacesName, ...
      'InitialValue', vSurfaceSelection, 'SelectionMode','multiple', ...
      'ListSize',[300 300], 'Name','Find Spots Close To Surface', ...
      'PromptString',{'Please select the surface:'});
  if vOk<1, return, end
end

vAnswer = inputdlg({'Please enter the threshold:'}, ...
    'Find Spots Close To Surface',1,{'5'});
if isempty(vAnswer), return, end
vThreshold = abs(str2double(vAnswer{1}));

vProgressDisplay = waitbar(0, 'Finding Spots Close To Surface');

vWorkingDataSet = CreateWorkingDataSet(vImarisApplication, Imaris.tType.eTypeFloat);
vSize = [vWorkingDataSet.GetSizeX(), vWorkingDataSet.GetSizeY(), vWorkingDataSet.GetSizeZ()];
vMin = [vWorkingDataSet.GetExtendMinX(), vWorkingDataSet.GetExtendMinY(), vWorkingDataSet.GetExtendMinZ()];
vMax = [vWorkingDataSet.GetExtendMaxX(), vWorkingDataSet.GetExtendMaxY(), vWorkingDataSet.GetExtendMaxZ()];
vVoxelSize = (vMax - vMin) ./ vSize;
 
vSurfaceProxyList = GetSurfaceProxyList(vImarisApplication, vParent, vSurfacesList, vSurfaceSelection);
vSpotsProxyList = GetSpotsProxyList(vImarisApplication, vParent, vSpotsList, vSpotsSelection);

vNumberOfSurfaceComponents = numel(vSurfaceProxyList);
vNumberOfSpotsComponents = numel(vSpotsProxyList);

vSurfaceTimeIndices = GetTimeIndicesToProcess(vImarisApplication.GetDataSet(), vSurfaceProxyList);

vNumberOfTimeIndices = size(vSurfaceTimeIndices, 2);

vSpotsCloseIndices = cell(vNumberOfSurfaceComponents, vNumberOfSpotsComponents);
vSpotsFarIndices = cell(vNumberOfSurfaceComponents, vNumberOfSpotsComponents);

vProgressValue = 0;
vProgressStep = 0.5 / (vNumberOfTimeIndices * vNumberOfSurfaceComponents ...
    * vNumberOfSpotsComponents);

% iterate over all timepoints
for vTimeIndex = 1:vNumberOfTimeIndices
  vTimeIndexBase0 = vTimeIndex - 1;
  for vSurfaceIndex = 1:vNumberOfSurfaceComponents
    if (vSurfaceTimeIndices(vSurfaceIndex, vTimeIndex) == 1)
      vSurfaceMaskGenerated = false;
      for vSpotsIndex = 1:vNumberOfSpotsComponents
        vProgressValue = vProgressValue + vProgressStep;
        waitbar(vProgressValue, vProgressDisplay, 'Calculating Object Distances');
        vSpots = vSpotsProxyList{vSpotsIndex};
        vSpotsTimeIndices = vSpots.GetIndicesT();
        vVoxelTimePointSelection = find(vSpotsTimeIndices == vTimeIndexBase0);
        if (~isempty(vVoxelTimePointSelection))
          % Generate distance transform from surface mask
          if ~vSurfaceMaskGenerated 
            vSurface = vSurfaceProxyList{vSurfaceIndex};
            vMaskImage = vSurface.GetMask(vMin(1), vMin(2), vMin(3), ...
                                          vMax(1), vMax(2), vMax(3), ...
                                          vSize(1), vSize(2), vSize(3), vTimeIndexBase0);
            
          
            for vIndexZ = 1:vSize(3)
              vSlice = (vMaskImage.GetDataSliceBytes(vIndexZ-1, 0, 0));
              vSliceInverted = (1 - vSlice);
              vWorkingDataSet.SetDataSliceFloats(vSliceInverted, vIndexZ-1, 0, 0);
            end
          
            % Distance Transform mask
            vInside = true;
            vIntensityThreshold = 1;
            vIP = vImarisApplication.GetImageProcessing;
            vWorkingDataSet = vIP.DistanceTransformDataSet(vWorkingDataSet, vIntensityThreshold, vInside);
            vSurfaceMaskGenerated = true;
          end
          
          vSpotsXYZ = vSpots.GetPositionsXYZ;
          vVoxelIndicesXYZ = GetVoxelIndices(vSpotsXYZ(vVoxelTimePointSelection,:), vMin, vVoxelSize);
          [vVoxelIndicesXYZSorted, vVoxelMapping] = sortrows(vVoxelIndicesXYZ, 3);
          vZSortedIntensities = GetVoxelIntensitiesByZSlice(vWorkingDataSet, vVoxelIndicesXYZSorted);
          vIntensities = zeros(size(vVoxelIndicesXYZ, 1), 1);
          vIntensities(vVoxelMapping) = vZSortedIntensities;
          vDistancesMin = vIntensities';

          vSpotsClose = find(vDistancesMin <= vThreshold);
          vSpotsCloseGlobalIndices = vVoxelTimePointSelection(vSpotsClose);
          vSpotsCloseIndices{vSurfaceIndex, vSpotsIndex} = ...
              [vSpotsCloseIndices{vSurfaceIndex, vSpotsIndex}; vSpotsCloseGlobalIndices];
          
          vSpotsFar = find(vDistancesMin > vThreshold);
          vSpotsFarGlobalIndices = vVoxelTimePointSelection(vSpotsFar);
          vSpotsFarIndices{vSurfaceIndex, vSpotsIndex} = ...
              [vSpotsFarIndices{vSurfaceIndex, vSpotsIndex}; vSpotsFarGlobalIndices]; 
          
        end
      end
    else
      % no surface at this timepoint,
      % copy all spots of current timepoint to the far spots component
      for vSpotsIndex = 1:vNumberOfSpotsComponents
        vSpots = vSpotsProxyList{vSpotsIndex};
        vSpotsTimeIndices = vSpots.GetIndicesT();
        vVoxelTimePointSelection = find(vSpotsTimeIndices == vTimeIndexBase0);
        vSpotsFarIndices{vSurfaceIndex, vSpotsIndex} = ...
            [vSpotsFarIndices{vSurfaceIndex, vSpotsIndex}; vVoxelTimePointSelection];
      end
    end
  end
end

vProgressValue = 0.7;
vProgressStep = 0.3 / (vNumberOfSurfaceComponents * vNumberOfSpotsComponents);

% generate new close and far spots components % Omit Far Spots 
for vSurfaceIndex = 1:vNumberOfSurfaceComponents
  for vSpotsIndex = 1:vNumberOfSpotsComponents
    vProgressValue = vProgressValue + vProgressStep;
    waitbar(vProgressValue, vProgressDisplay, 'Generating Spots Components');
        
    vSpotsSelection = vSpotsCloseIndices{vSurfaceIndex, vSpotsIndex};
    vSurface = vSurfaceProxyList{vSurfaceIndex};
    vSpots = vSpotsProxyList{vSpotsIndex};
    vSpotsXYZ = vSpots.GetPositionsXYZ;
    vSpotsRadius = vSpots.GetRadiiXYZ;
    vSpotsTime = vSpots.GetIndicesT();
      
    % close spots
    vName = sprintf('%s close to %s [%.2f]', ...
          char(vSpots.GetName), char(vSurface.GetName), vThreshold);
    vColor = 'ff00ff';
    vNewSpotsClose = CreateSelectedSpots(vImarisApplication, vSpotsXYZ, ...
        vSpotsRadius, vSpotsTime, vSpotsSelection, vColor, vName);
    vParent.AddChild(vNewSpotsClose, -1);
      
    % far spots
%     vSpotsSelection = vSpotsFarIndices{vSurfaceIndex, vSpotsIndex};
%     vName = sprintf('%s far from %s [%.2f]', ...
%       char(vSpots.GetName), char(vSurface.GetName), vThreshold);
%     vColor = 'ffff00';
%     vNewSpotsFar = CreateSelectedSpots(vImarisApplication, vSpotsXYZ, ...
%         vSpotsRadius, vSpotsTime, vSpotsSelection, vColor, vName);
%     vParent.AddChild(vNewSpotsFar, -1);
  end 
end

% Hide all old spots
for vSpotsIndex = 1:vNumberOfSpotsComponents
  vSpots = vSpotsProxyList{vSpotsIndex};
  vSpots.SetVisible(false);
end

close(vProgressDisplay);

end

function vDataSet = CreateWorkingDataSet(aImarisApplication, aType)
  vOrigDataSet = aImarisApplication.GetDataSet();
  vDataSet = aImarisApplication.GetFactory().CreateDataSet;
  vSizeX = vOrigDataSet.GetSizeX();
  vSizeY = vOrigDataSet.GetSizeY();
  vSizeZ = vOrigDataSet.GetSizeZ();
  vSizeC = 1;
  vSizeT = 1;
  vExtentMin = [vOrigDataSet.GetExtendMinX(), ...
      vOrigDataSet.GetExtendMinY(), vOrigDataSet.GetExtendMinZ()];
  vExtentMax = [vOrigDataSet.GetExtendMaxX(), ...
      vOrigDataSet.GetExtendMaxY(), vOrigDataSet.GetExtendMaxZ()];
  vDataSet.Create(aType, vSizeX, vSizeY, vSizeZ, vSizeC, vSizeT);
  vDataSet.SetExtendMinX(vExtentMin(1));
  vDataSet.SetExtendMinY(vExtentMin(2));
  vDataSet.SetExtendMinZ(vExtentMin(3));
  vDataSet.SetExtendMaxX(vExtentMax(1));
  vDataSet.SetExtendMaxY(vExtentMax(2));
  vDataSet.SetExtendMaxZ(vExtentMax(3));
end

function vVoxelIndicesXYZ = GetVoxelIndices(aPositionsXYZ, aMin, aVoxelSize)
  vNumberOfPositions = size(aPositionsXYZ, 1);
  vVoxelIndicesXYZ = floor((aPositionsXYZ - ...
      ones(vNumberOfPositions, 1) * aMin) * diag(1 ./ aVoxelSize)');
end

function vIntensities = GetVoxelIntensitiesByZSlice(aDataSet, aVoxelIndicesXYZSorted)
  vNumberOfIndices = size(aVoxelIndicesXYZSorted,1);
  vIntensities = zeros(vNumberOfIndices,1);
  vSizeX = aDataSet.GetSizeX();
  vSizeY = aDataSet.GetSizeY();
  vStartX = 0;
  vStartY = 0;
  vSizeZ = 1;
  vIndexC = 0;
  vIndexT = 0;
    
  vRowStart = 1;
  vNewZSlice = aVoxelIndicesXYZSorted(1,3);
  vCurrentZSliceIndex = vNewZSlice;
  for vRowIndex = 2:vNumberOfIndices+1
    if vRowIndex ~= vNumberOfIndices + 1
        vNewZSlice = aVoxelIndicesXYZSorted(vRowIndex,3);
    end
    if vNewZSlice ~= vCurrentZSliceIndex || vRowIndex == vNumberOfIndices + 1
      vCurrentZSliceIndex = vNewZSlice;
      vRowEnd = vRowIndex-1;
      vRows = vRowStart:vRowEnd;
      vRowStart = vRowIndex;
      vIndices = aVoxelIndicesXYZSorted(vRows,:);
      vVoxelDataSlice = aDataSet.GetDataSubVolumeAs1DArrayFloats(vStartX, ...
          vStartY, uint32(vCurrentZSliceIndex), vIndexC, vIndexT, vSizeX, vSizeY, vSizeZ);
      vIntensities(vRows) = vVoxelDataSlice(vIndices(:,1) + 1 + vIndices(:,2) * vSizeX);
    end
  end
end

function vSurfaceProxyList = GetSurfaceProxyList(aImarisApplication, ...
    aParent, aSurfacesList, aSurfaceSelection)
  vNumberOfSurfacesSelected = numel(aSurfaceSelection);
  vSurfaceProxyList = cell(vNumberOfSurfacesSelected, 1);
  for vSurfaceIndex = 1:vNumberOfSurfacesSelected
    vItem = aParent.GetChild(aSurfacesList( ...
        aSurfaceSelection(vSurfaceIndex)) - 1);
    vSurface = aImarisApplication.GetFactory.ToSurfaces(vItem);
    vSurfaceProxyList{vSurfaceIndex} = vSurface;
  end
end

function vSpotsProxyList = GetSpotsProxyList(aImarisApplication, aParent, ...
    aSpotsList, aSpotsSelection)
  vNumberOfSpotsSelected = numel(aSpotsSelection);
  vSpotsProxyList = cell(vNumberOfSpotsSelected, 1);
  for vSurfaceIndex = 1:vNumberOfSpotsSelected
    vItem = aParent.GetChild(aSpotsList( ...
        aSpotsSelection(vSurfaceIndex)) - 1);
    vSpot = aImarisApplication.GetFactory.ToSpots(vItem);
    vSpotsProxyList(vSurfaceIndex) = vSpot;
  end
end

function vSurfaceTimeIndices = GetTimeIndicesToProcess(aDataSet, aSurfacesList)
  vNumberOfTimepoints = aDataSet.GetSizeT();
  vNumberOfSurfaces = numel(aSurfacesList);    
  vSurfaceTimeIndices = zeros(vNumberOfSurfaces, vNumberOfTimepoints);
  for vSurfaceIndex = 1:vNumberOfSurfaces
    vSurface = aSurfacesList{vSurfaceIndex};
    vNumberOfSurfaces = vSurface.GetNumberOfSurfaces();
    for vI = 1:vNumberOfSurfaces
      vTimeIndex = vSurface.GetTimeIndex(vI-1);
      vSurfaceTimeIndices(vSurfaceIndex, vTimeIndex+1) = 1;
    end
  end
end

function vNewSpots = CreateSelectedSpots(aImarisApplication, aSpotsXYZ, ...
  aSpotsRadius, aSpotsTime, aSpotsSelection, aColor, aName)
  vNewSpots = aImarisApplication.GetFactory.CreateSpots;
  if ~isempty(aSpotsSelection)
    vNewSpots.Set(aSpotsXYZ(aSpotsSelection, :), ...
    aSpotsTime(aSpotsSelection), aSpotsRadius(aSpotsSelection,1));
    vNewSpots.SetRadiiXYZ(aSpotsRadius(aSpotsSelection,:));
  end
  vNewSpots.SetColorRGBA(hex2dec(aColor));
  vNewSpots.SetName(aName);
end
