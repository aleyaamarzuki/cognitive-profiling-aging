%% Script for obtaining GMV estimates of Regions-of-Interest (ROIs) %%
clc; clear;

%get SPM12
addpath 'C:\Users\aleya\OneDrive\Documents\spm12';
spm


% Define paths
mask_path = 'C:\Users\aleya\OneDrive\Desktop\Cog Modelling Ageing Project\CleanDataFiles\brain_masks\';  
data_path = 'D:\397Participants\';

% Get names of masks
mask_files1 = dir(mask_path);
mask_files = mask_files1(~[mask_files1.isdir]);

files1 = dir(data_path);

%remove elements that are not participant folders
%files = files1(~[files1.isdir]);
idx = ismember({files1([files1.isdir]).name}, {'.','..','*.txt'});
files = files1(~idx);

subject_ids = cellstr(vertcat(files.name));
mask_names = {mask_files.name}'; %get mask names in one file

% Initialize SPM
spm('Defaults','fMRI');
spm_jobman('initcfg');

% Initialize an array to store the volumes
volumes = zeros(length(subject_ids), length(mask_files));

for m = 1:length(mask_files)

% Load the mask
curr_mask_path = [mask_path mask_names{m}]; %current mask 
mask_img = spm_vol(curr_mask_path);
mask_data = spm_read_vols(mask_img);

% Loop through each subject
for i = 1:length(subject_ids)
    subject_id = subject_ids{i};
    subject_file = fullfile([data_path subject_id '\c1' subject_id '.nii']);
    
    % Load the subject image
    subject_img = spm_vol(subject_file);
    subject_data = spm_read_vols(subject_img);
    
    % Ensure mask and subject image have the same dimensions
    if ~isequal(size(subject_data), size(mask_data))
        % Reslice the mask to match the subject image dimensions
        flags = struct('mask', true, 'mean', false, 'interp', 1, 'which', 1, 'wrap', [0 0 0], 'prefix', 'r');
        spm_reslice({subject_file, curr_mask_path}, flags);
        
        % Load the resliced mask
        [pathstr, name, ext] = fileparts(curr_mask_path);
        resliced_mask_path = fullfile(pathstr, ['r' name ext]);
        resliced_mask_img = spm_vol(resliced_mask_path);
        resliced_mask_data = spm_read_vols(resliced_mask_img);
    else
        resliced_mask_data = mask_data;
    end
    
    % Apply the mask
    masked_data = subject_data .* resliced_mask_data;
    
    % Calculate the voxel volume (in mm³)
    voxel_volume = abs(det(subject_img.mat(1:3, 1:3)));
    
    % Calculate the volume within the mask
    roi_volume = sum(masked_data(:) > 0) * voxel_volume;  % Count non-zero voxels and multiply by voxel volume

    % Display the result
    fprintf('Subject: %s, ROI Volume: %.2f mm^3\n', subject_id, roi_volume);
   
    
    % Store the volume for this subject
    volumes(i,m) = roi_volume;

    % Clean up temporary resliced mask file if it was created
    if exist('resliced_mask_path', 'var')
        delete(resliced_mask_path);
    end
end

end 

roi_vol = table(subject_ids, volumes(:,1),volumes(:,2),...
    volumes(:,3), volumes(:,4), volumes(:,5), volumes(:,6),... 
    volumes(:,7), volumes(:,8));

roi_vol.Properties.VariableNames{'Var2'} = 'Amygdala';
roi_vol.Properties.VariableNames{'Var3'} = 'Cerebellum';
roi_vol.Properties.VariableNames{'Var4'} = 'DLPFC';
roi_vol.Properties.VariableNames{'Var5'} = 'Hippo';
roi_vol.Properties.VariableNames{'Var6'} = 'IFG';
roi_vol.Properties.VariableNames{'Var7'} = 'Insula';
roi_vol.Properties.VariableNames{'Var8'} = 'PFC';
roi_vol.Properties.VariableNames{'Var9'} = 'SuppMotor';


writetable(roi_vol, 'C:\Users\aleya\OneDrive\Desktop\Cog Modelling Ageing Project\CleanDataFiles\roi_vol.csv'); 