-- !preview conn=DBI::dbConnect(odbc::odbc(), "iquizoo-v3", database = "iquizoo_user_db")
SELECT
	user_profile.Id user_id,
	user_profile.RealName user_name,
	user_profile.Gender user_sex,
	user_profile.Birthday user_dob,
	base_organization.Id org_id,
	base_organization.`Name` school,
	base_grade_class.GradeName grade,
	base_grade_class.ClassName class
FROM
	user_profile
	INNER JOIN base_organization ON base_organization.Id = user_profile.OrgId
	INNER JOIN base_grade_class ON base_grade_class.Id = user_profile.ClassId
WHERE
	base_organization.`Name` IN ( {organization_names} );
