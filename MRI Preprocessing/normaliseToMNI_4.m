
%% Normalise to MNI

clc; clear;

data_path = 'C:\Users\aleya\Documents\MRI\Data\SMCV\';

%get foldernames
files1 = dir(data_path);

%remove elements that are not participant folders
matches = regexp({files1([files1.isdir]).name}, {'^\d+|^A\d+'});
bool = cellfun(@(x) any(x), matches);
files = files1(bool);

%get SPM12
addpath 'C:\Users\aleya\Documents\spm12\';
spm


%Script folder
addpath 'C:\Users\aleya\Documents\MRI\Scripts'

% generate file names and directories to feed into Dartel
for i = 1:length(files)
subnum = files(i).name; %get names of each subject's folder
%u_rc1 templates
u_rc1Files{i,1} = [data_path num2str(subnum) '\mprage\anat\u_rc1' num2str(subnum) '_Template.nii']; 

%c1 files
c1Files{1,1}{i,1} = [data_path num2str(subnum) '\mprage\anat\c1' num2str(subnum) '.nii']; 
end


%stored in first subject's folder
overall_template = {[data_path files(2).name '\mprage\anat\Template_6.nii']};


%% Run Normalisation
%parameter settings based on: https://www.fil.ion.ucl.ac.uk/~john/misc/VBMclass15.pdf

matlabbatch{1}.spm.tools.dartel.mni_norm.template = overall_template;
matlabbatch{1}.spm.tools.dartel.mni_norm.data.subjs.flowfields = u_rc1Files;
matlabbatch{1}.spm.tools.dartel.mni_norm.data.subjs.images = c1Files;


matlabbatch{1}.spm.tools.dartel.mni_norm.vox = [NaN NaN NaN]; %Default: 1.5 x 1.5 x 1.5
matlabbatch{1}.spm.tools.dartel.mni_norm.bb = [NaN NaN NaN
                                               NaN NaN NaN]; %bounding box default
matlabbatch{1}.spm.tools.dartel.mni_norm.preserve = 1; %Preserve Amount
matlabbatch{1}.spm.tools.dartel.mni_norm.fwhm = [8 8 8]; %FWHM smoothing 8mm

spm_jobman('run', matlabbatch);