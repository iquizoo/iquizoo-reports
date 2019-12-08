-- !preview conn=DBI::dbConnect(odbc::odbc(), "iquizoo-v3", database = "iquizoo_datacenter_db")
SELECT
	content_score_detail.OrganizationId org_id,
	content_score_detail.UserId user_id,
	iquizoo_content_db.content.`Name` game_name,
	content_score_detail.CreateTime game_time,
	ab_ref.AbilityCode ab_code,
	ab_ref.AbilityName ab_name,
	content_score_detail.ApproximateScore raw_score
FROM
	content_score_detail
	INNER JOIN iquizoo_user_db.base_organization ON iquizoo_user_db.base_organization.Id = content_score_detail.OrganizationId
	INNER JOIN iquizoo_content_db.content ON content_score_detail.ContentId = iquizoo_content_db.content.Id
	INNER JOIN ( SELECT DISTINCTROW AbilityId, AbilityName, AbilityCode FROM iquizoo_report_db.ability_reference ) ab_ref ON ab_ref.AbilityId = iquizoo_content_db.content.FirstAbilityId
WHERE
	base_organization.`Name` IN ( {organization_names} );
