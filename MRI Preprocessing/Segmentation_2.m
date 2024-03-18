%% performs segmentation
clc;
clear;

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

%parameter settings based on: https://www.fil.ion.ucl.ac.uk/~john/misc/VBMclass15.pdf

for i = 1:length(files) %this is where segmentation starts

subnum = files(i).name; %get names of each subject's folder

matlabbatch{1}.spm.spatial.preproc.channel.vols = {[data_path num2str(subnum) '\mprage\anat\' num2str(subnum) '.nii,1']};
matlabbatch{1}.spm.spatial.preproc.channel.biasreg = 0.001; %bias regularisation (default)
matlabbatch{1}.spm.spatial.preproc.channel.biasfwhm = 60; 
matlabbatch{1}.spm.spatial.preproc.channel.write = [0 0];
%Tissue prob map corresponding to GMV
matlabbatch{1}.spm.spatial.preproc.tissue(1).tpm = {'C:\Users\aleya\Documents\spm12\tpm\TPM.nii,1'};
matlabbatch{1}.spm.spatial.preproc.tissue(1).ngaus = 1; 
matlabbatch{1}.spm.spatial.preproc.tissue(1).native = [1 1];
matlabbatch{1}.spm.spatial.preproc.tissue(1).warped = [0 0];
%White Matter
matlabbatch{1}.spm.spatial.preproc.tissue(2).tpm = {'C:\Users\aleya\Documents\spm12\tpm\TPM.nii,2'};
matlabbatch{1}.spm.spatial.preproc.tissue(2).ngaus = 1;
matlabbatch{1}.spm.spatial.preproc.tissue(2).native = [1 1];
matlabbatch{1}.spm.spatial.preproc.tissue(2).warped = [0 0];
%SCF
matlabbatch{1}.spm.spatial.preproc.tissue(3).tpm = {'C:\Users\aleya\Documents\spm12\tpm\TPM.nii,3'};
matlabbatch{1}.spm.spatial.preproc.tissue(3).ngaus = 2;
matlabbatch{1}.spm.spatial.preproc.tissue(3).native = [1 0];
matlabbatch{1}.spm.spatial.preproc.tissue(3).warped = [0 0];
%Skull
matlabbatch{1}.spm.spatial.preproc.tissue(4).tpm = {'C:\Users\aleya\Documents\spm12\tpm\TPM.nii,4'};
matlabbatch{1}.spm.spatial.preproc.tissue(4).ngaus = 3;
matlabbatch{1}.spm.spatial.preproc.tissue(4).native = [0 0];
matlabbatch{1}.spm.spatial.preproc.tissue(4).warped = [0 0];
%Soft tissue outside brain
matlabbatch{1}.spm.spatial.preproc.tissue(5).tpm = {'C:\Users\aleya\Documents\spm12\tpm\TPM.nii,5'};
matlabbatch{1}.spm.spatial.preproc.tissue(5).ngaus = 4;
matlabbatch{1}.spm.spatial.preproc.tissue(5).native = [0 0];
matlabbatch{1}.spm.spatial.preproc.tissue(5).warped = [0 0];
%Air
matlabbatch{1}.spm.spatial.preproc.tissue(6).tpm = {'C:\Users\aleya\Documents\spm12\tpm\TPM.nii,6'};
matlabbatch{1}.spm.spatial.preproc.tissue(6).ngaus = 2;
matlabbatch{1}.spm.spatial.preproc.tissue(6).native = [0 0];
matlabbatch{1}.spm.spatial.preproc.tissue(6).warped = [0 0];
matlabbatch{1}.spm.spatial.preproc.warp.mrf = 1;
matlabbatch{1}.spm.spatial.preproc.warp.cleanup = 1;
matlabbatch{1}.spm.spatial.preproc.warp.reg = [0 0.001 0.5 0.05 0.2];
matlabbatch{1}.spm.spatial.preproc.warp.affreg = 'eastern'; %change to european if looking at Western sample
matlabbatch{1}.spm.spatial.preproc.warp.fwhm = 0;
matlabbatch{1}.spm.spatial.preproc.warp.samp = 3;
matlabbatch{1}.spm.spatial.preproc.warp.write = [0 0];
matlabbatch{1}.spm.spatial.preproc.warp.vox = NaN;
matlabbatch{1}.spm.spatial.preproc.warp.bb = [NaN NaN NaN
                                              NaN NaN NaN];
spm_jobman('run', matlabbatch);

end