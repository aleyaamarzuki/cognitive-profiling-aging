clc; clear;

%overall folder path. add path containing masks and smwc1 folders here
data_path = 'C:\Users\aleya\Documents\MRI\';

%asks whether task to be analysed is gng or wcst
task_type =  input('Which task? gng or wcst?', 's'); 

%asks whether you want to add just a single variable or multi
typevar = input('Single var or multi?','s');

%read in spreadsheet with behavioural data
info = readtable([data_path 'globals_' task_type '.csv']);

%calculate global intracranial volume per participant to be used as
%regressor
info.TotalVolume = info.GreyMatter + info.WhiteMatter + info.CSF;

%convert ethnicity to numerical categories
info.Ethnicity = grp2idx(categorical(info.Ethnicity));

files1 = dir([data_path 'smwc1_files']);

%remove elements that are not participant folders
matches = regexp({files1([files1.isdir]).name}, {'^\d+|^A\d+'});
bool = cellfun(@(x) any(x), matches);
files = files1(bool);

%ask user which var from info file to include
%user needs to add variables in this format {'PC1', 'PC2',.. etc}
variable =  input(' Which variable(s) would you like to include? If there are multiple, add them into a {} and separate using a comma');
if strcmp(typevar, "multi")
    varname = [variable{1,1} variable{1,2}];
else
    varname = variable{1,1};
end
mkdir([data_path 'Results\' task_type '\' typevar '\' varname]); %create folder to store results using variable and task name

%ask user whether they want to mask the data (1 = Yes, 2 = No)
mask_choice =  input('Would you like to apply a mask? Press 1 for Yes and 2 for No.', 's'); 

mask_choice = str2num(mask_choice);

% if they say yes to wanting a mask, ask them to input which mask they
% would like (make sure the mask file is in your Results folder)
if mask_choice == 1
    mask =  input('Which region would you like to apply a mask for?', 's'); 
    mkdir([data_path 'Results\' task_type '\'  typevar '\' varname '\' mask]); %create folder to store results using variable name
else 
end

%get file names in one file from cell to struct
swmc1 = {files_mri.name}';

%flag files with NA values in variable(s) of choice
for m = 1:length(variable)
    na_idx = isnan(info.(variable{1,m}));
    swmc1 = swmc1(~na_idx);%get only scans that correspond to non-NA values
    info = info(~na_idx,:); %trim info table to remove NA values
end

%add scans to file in this format because we need to read this into spm
%later
for i = 1:length(swmc1)

    scans{i,1} = [data_path 'smwc1_files\' swmc1{i}];

end


%get SPM12
addpath 'C:\Users\aleya\Documents\spm12\';
spm

%% conduct analysis %%

%% data path depending on whether there masking is done or not
if mask_choice == 1
   matlabbatch{1}.spm.stats.factorial_design.dir = {[data_path 'Results\' task_type '\'  typevar '\' varname  '\' mask]};
elseif mask_choice == 2
   matlabbatch{1}.spm.stats.factorial_design.dir = {[data_path 'Results\' task_type '\' typevar '\' varname]};
end

%% read in data
matlabbatch{1}.spm.stats.factorial_design.des.mreg.scans = scans;

%% Control for MRI scanner (SunMed or SMCV) 
matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(1).c = info.MRI;
matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(1).cname = 'MRI';
matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(1).iCC = 1;

%% control for ethnicity
matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(2).c = info.Ethnicity;
matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(2).cname = 'Ethnicity';
matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(2).iCC = 1;

%% control for Age if Age is not the variable of interest
matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(3).c = info.Age;
matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(3).cname = 'Age';
matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(3).iCC = 1;

%% control for Gender %%
matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(4).c = info.GenderScore;
matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(4).cname = 'Gender';
matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(4).iCC = 1;

%% add variable of choice
if strcmp(typevar, 'multi') %create multiple IVs if user requests
for n = 1:length(variable)
    matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(4+n).c = info.(variable{1,n});
    matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(4+n).cname = variable{1,n};
    matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(4+n).iCC = 1;
end
else 
    matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(5).c = info.(variable{1,1});
    matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(5).cname = variable{1,1};
    matlabbatch{1}.spm.stats.factorial_design.des.mreg.mcov(5).iCC = 1; 
end 
%% masking and thresholding
matlabbatch{1}.spm.stats.factorial_design.des.mreg.incint = 1;
matlabbatch{1}.spm.stats.factorial_design.cov = struct('c', {}, 'cname', {}, 'iCFI', {}, 'iCC', {});
matlabbatch{1}.spm.stats.factorial_design.multi_cov = struct('files', {}, 'iCFI', {}, 'iCC', {});

if mask_choice == 1
matlabbatch{1}.spm.stats.factorial_design.masking.im = 0;
%make sure masks are in your datapath
matlabbatch{1}.spm.stats.factorial_design.masking.em = {[data_path mask '.nii,1']};

elseif mask_choice == 2
matlabbatch{1}.spm.stats.factorial_design.masking.tm.tma.athresh = 0.2;
matlabbatch{1}.spm.stats.factorial_design.masking.im = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.em = {''};
end

%% read in globals
matlabbatch{1}.spm.stats.factorial_design.globalc.g_user.global_uval = info.TotalVolume;
%% use globals as nuisance regressor
matlabbatch{1}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.glonorm = 3;


%run estimation

if mask_choice == 1
   matlabbatch{2}.spm.stats.fmri_est.spmmat = {[data_path 'Results\' task_type '\'  typevar '\' varname '\' mask '\SPM.mat']};
elseif mask_choice == 2
   matlabbatch{2}.spm.stats.fmri_est.spmmat = {[data_path 'Results\' task_type '\'  typevar '\' varname '\SPM.mat']};
end

matlabbatch{2}.spm.stats.fmri_est.write_residuals = 1;
matlabbatch{2}.spm.stats.fmri_est.method.Classical = 1;

% create contrasts automatically
if mask_choice == 1
    matlabbatch{3}.spm.stats.con.spmmat = {[data_path 'Results\' task_type '\'  typevar '\' varname '\' mask '\SPM.mat']};
elseif mask_choice == 2
     matlabbatch{3}.spm.stats.con.spmmat = {[data_path 'Results\' task_type '\'  typevar '\' varname '\SPM.mat']};
end 

if strcmp(typevar, 'single') %contrasts if univariate
   matlabbatch{3}.spm.stats.con.consess{1}.tcon.name = 'pos';
   matlabbatch{3}.spm.stats.con.consess{1}.tcon.convec = [0 0 0 0 0 1 0];
   matlabbatch{3}.spm.stats.con.consess{2}.tcon.name = 'neg';
   matlabbatch{3}.spm.stats.con.consess{2}.tcon.convec = [0 0 0 0 0 -1 0];
elseif strcmp(typevar, 'multi') %contrasts if multivariate
   matlabbatch{3}.spm.stats.con.consess{1}.tcon.name = 'posFirst';
   matlabbatch{3}.spm.stats.con.consess{1}.tcon.convec = [0 0 0 0 0 1 -1 0];
   matlabbatch{3}.spm.stats.con.consess{2}.tcon.name = 'negFirst';
   matlabbatch{3}.spm.stats.con.consess{2}.tcon.convec = [0 0 0 0 0 -1 1 0];

end 

%run all jobs
spm_jobman('run', matlabbatch);

